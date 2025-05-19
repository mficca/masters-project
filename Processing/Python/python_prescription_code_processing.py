#!/usr/bin/env python3
import argparse
import pandas as pd
import numpy as np
import re
import csv
from pathlib import Path

def main(write_to_data_example: bool, sample_data: bool):

    #Determine repo root
    base_dir = Path(__file__).resolve().parents[2]

    #get input data csv from Postgres sql pull or sample data
    if sample_data:
        pmt_path = base_dir / "Postgres" / "results" / "sample_data.csv"
    else:
        pmt_path = base_dir / "Postgres" / "results" / "Patient_Matrix_Pull_Prescriptions_Patient_Type.csv"

    # choose output directory
    if write_to_data_example:
        out_dir = base_dir / "data_example"
    else:
        out_dir = base_dir / "Processing" / "outputs" / "python"

    print("Writing to directory - ", out_dir)

    #create the output directory if doesnt exist
    out_dir.mkdir(parents=True, exist_ok=True)

    #1- Read in and process the data ----------------------------------------------
    df = pd.read_csv(pmt_path, parse_dates=["startdate"])

    # change these prescription codes to be 
    df["formulary_drug_cd"] = df["formulary_drug_cd"].replace({
        "1/4NS1000":  "NS",
        "5000MLBAG":  "MLBAG",
        "0.45NS3000I":"NS",
        "5FU500I":    "FU",
        "1/2NS250I":  "NS",
    })

    # drop blanks & newborns
    df = (
        df.assign(formulary_drug_cd = df["formulary_drug_cd"].astype(str).str.strip())
          .query("formulary_drug_cd != ''")
          .query("admission_type != 'NEWBORN'")
    )

    print("Finished Step 1 - Read in and processed data")

    #2 DERIVE new_drug_cd & letter_check ----------------------------------------------
    # strip digits + anything after, then remove periods, then slash→Q
    def make_new_code(cd):
        # remove digits and beyond
        base = re.sub(r"[0-9]+.*$", "", cd)
        # drop periods
        base = base.replace(".", "")
        # chages slashes to Q
        return base.replace("/", "Q")

    #create new drug code col
    df["new_drug_cd"]  = df["formulary_drug_cd"].map(make_new_code)
    
    #create flag for letter check True if only letters in string for new drug code col
    df["letter_check"] = df["new_drug_cd"].str.fullmatch(r"[A-Za-z]+")

    # sort values so events are in date order for later processing
    df = df.sort_values(["subject_id", "startdate"])

    print("Finished Step 2 - Derived new drug codes")

    #3- UNIQUE (subject, date, drug) with randomized same day -------------------------
    df1 = (
        df[["subject_id","startdate","new_drug_cd"]]
            .drop_duplicates(subset=["subject_id","startdate","new_drug_cd"])
            .sort_values(["subject_id","startdate"])
            .groupby(["subject_id","startdate"], group_keys=False)
            .apply(lambda g: g.sample(frac=1))
            .reset_index(drop=True)
    )

    

    print("Finished Step 3 - Returned unique prescriptions by day and randomized sequence")

    #4- filter by drug count (between 5 and 50,0000) --------------------------------
    drug_counts = (
        df1.groupby("new_drug_cd")
           .size()
           .rename("count")
           .reset_index()
    )
    
    #get and filter for drugs that where count between 5 & 50000
    valid_drugs = drug_counts.query("5 <= count <= 50000")["new_drug_cd"]
    df2 = df1[df1["new_drug_cd"].isin(valid_drugs)]

    print("Finished Step 4 - Filtered for drugs with overall count 5 - 50000")  

    #5- filter for patients with between 8 and 64 prescriptions ----------------------------
    pat_counts = (
        df2.groupby("subject_id")
           .size()
           .rename("count")
           .reset_index()
    )
    valid_pats = pat_counts.query("8 <= count <= 64")["subject_id"]
    df3 = df2[df2["subject_id"].isin(valid_pats)]

    print("Finished Step 5 - Filtered for patients between 8 and 64 prescriptions")

    #6- Assign drug ids to the new drug id code ----------------------------------------------
    unique_drugs = sorted(df3["new_drug_cd"].unique())
    id_map = {drug: idx+1 for idx, drug in enumerate(unique_drugs)}
    formulary_ids = pd.DataFrame({
        "new_drug_cd": unique_drugs,
        "Drug_ID": [id_map[d] for d in unique_drugs]
    })

    print("Finished Step 6 - Assigned new drug id code")

    #merge dfs
    df4 = df3.merge(formulary_ids, on="new_drug_cd")

    #7- Build patient prescription sequence table/vectors
    pats = []
    for subj, grp in df4.groupby("subject_id", sort=True):
        seq = list(dict.fromkeys(grp["Drug_ID"]))
        pats.append([f"pat_{subj}", ", ".join(map(str, seq))])

    cohort = pd.DataFrame(pats, columns=["mrn", "EHRseq"])


    print("Finished Step 7 - Built patient prescription table sequences")

    #8- Get Train/Test split at 5:1 ------------------------------------------------
    train = cohort.sample(frac=5/6, random_state=40)
    test  = cohort.drop(train.index).reset_index(drop=True)
    train = train.reset_index(drop=True)

    print("Finished Step 8 - Created train and test sets")

    #9- Create vocab cohort ---------------------------------------------------------
    vocab = pd.DataFrame({
    "new_drug_cd": unique_drugs,
    "CODE":        list(range(1, len(unique_drugs) + 1))
    })

    # map labesl to codes
    vocab["LABEL"] = vocab["CODE"].map(lambda i: f"cl_term_{i}")
    
    # reordering
    vocab = vocab[["LABEL", "CODE", "new_drug_cd"]]

    print("Finished Step 9 - Created vocab cohort")

    #10- Write outputs to appropriate directory -------------------------------------
    # ALL PATIENTS
    with open(out_dir / "All_Patients.csv", "w") as f:
        f.write("mrn,EHRseq\n")
        for mrn, seq in cohort.values:
            f.write(f"{mrn},{seq}\n")

    # TRAINING SET
    with open(out_dir / "cohort-ehrseq.csv", "w") as f:
        f.write("mrn,EHRseq\n")
        for mrn, seq in train.values:
            f.write(f"{mrn},{seq}\n")
    
    # TEST SET
    with open(out_dir / "cohort_test-ehrseq.csv", "w") as f:
        f.write("mrn,EHRseq\n")
        for mrn, seq in test.values:
            f.write(f"{mrn},{seq}\n")

    #VOCAB
    vocab.to_csv(out_dir / "cohort-vocab.csv",    index=False)


    print(f"Wrote 4 files into {out_dir}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Process prescription data; use --data-example to write to data_example."
    )
    parser.add_argument(
        "--data-example",
        action="store_true",
        dest="write_to_data_example",
        help="Write outputs to the data_example directory instead of Processing/outputs/python"
    )
    parser.add_argument(
        "--sample-data",
        action="store_true",
        dest="sample_data",
        help="Read in sample data instead of real data from Postgres/results directory"
    )
    args = parser.parse_args()
    main(args.write_to_data_example, args.sample_data)