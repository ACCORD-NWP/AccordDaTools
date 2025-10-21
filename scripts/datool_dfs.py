#!/usr/bin/env python3

import sys
import argparse
import shlex
import numpy as np
import matplotlib.pyplot as plt

import pyodc as odc

numvar = 29
valdfs = np.zeros(numvar)
numdfs = np.zeros(numvar, dtype=int)
cntnot = 0
cntdfs = 0

def parse_args():
    parser = argparse.ArgumentParser(
        description="Compare DFS from perturbed and unperturbed CCMA ODB queries.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument('--file1', help='ODB-2/ASCII file from perturbed CCMA')
    parser.add_argument('--file2', help='ODB-2/ASCII file from unperturbed CCMA')
    parser.add_argument('--write-dfs', action='store_true', help='Write raw DFS data to dfs.dat')
    parser.add_argument('--plot', nargs='?', const='percent', choices=['raw', 'perobs', 'percent'],
        help="Generate a DFS plot:\n"
             "  percent - percentage contribution (default)\n"
             "  perobs  - DFS per observation\n"
             "  raw     - total DFS\n"
    )
    parser.add_argument('--plot-style', help='Matplotlib style for plotting', default='default')
    parser.add_argument('--list-plot-styles', action='store_true', help='List available matplotlib plot styles and exit')

    # Check if no arguments at all
    if len(sys.argv) == 1:
        print(f"datool_dfs: Error: No arguments provided.")
        parser.print_usage()
        sys.exit(1)

    return parser.parse_args()


def is_odb_file(path):
    try:
        with open(path, 'rb') as f:
            magic = f.read(5)
            # Check for uint16 0xFFFF followed by ASCII "ODA"
            return magic[:2] == b'\xFF\xFF' and magic[2:] == b'ODA'
    except Exception:
        pass

    try:
        odc.Reader(path).columns
        return True
    except Exception:
        return False

def is_ascii_file(path):
    try:
        with open(path, 'r', encoding='utf-8') as f:
            # Read first non-empty, non-comment line
            for line in f:
                if line.strip() and not line.strip().startswith("#"):
                    parts = line.strip().split()
                    return len(parts) >= 12  # your ASCII reader expects 12 fields
    except Exception:
        return False
    return False

def read_odb_file(path):
    import pandas as pd

    # Read the full table using odc.Reader
    try:
        df = odc.read_odb(path,single=True)
    except Exception as e:
        raise RuntimeError(f"Failed to read ODB file {path} using odc: {e}")

    # Filter the data like odbsql
    FG2BIG_MASK = 1 << 8
#    FG2BIG_MASK = 1 << 20
    filtered_df = df[
        (df['varno@body'] != 91) &
        (df['an_depar@body'].notnull()) &
        ((df['datum_event1@body'] & FG2BIG_MASK) == 0)
    ]
    df = filtered_df
    # Expected column names
    expected_columns = [
        'obstype@hdr', 'codetype@hdr', 'vertco_reference_1@body', 'sensor@hdr', 'statid@hdr',
        'varno@body', 'lat@hdr', 'lon@hdr', 'obsvalue@body', 'final_obs_error@errstat',
        'fg_depar@body', 'an_depar@body'
    ]

    # Verify all required columns exist
    missing = [col for col in expected_columns if col not in df.columns]
    if missing:
        raise ValueError(f"Missing required columns in ODB file {path}: {', '.join(missing)}")

    # Return list of records as dictionaries
    return df[expected_columns].to_dict(orient='records')


def determine_index(obstyp, varno, codetype, sensor, ichan, statid):
    # Return observation type index (1-based)
    if obstyp == 1:
        if varno == 1: return 1   # SYNOP_Z
        if varno == 39: return 2  # SYNOP_T2
        if varno in [58, 40]: return 3  # SYNOP_R2
        if varno in [41, 42]: return 4  # SYNOP_U10
        if varno == 128: return 5  # GNSS_ZTD
    if obstyp == 5:
        if varno in [3, 4, 41, 42]: return 6  # TEMP_U
        if varno in [2, 39]: return 7  # TEMP_T
        if varno == 1: return 8  # TEMP_Z
        if varno in [58, 59, 40, 7]: return 9  # TEMP_Q
        if varno == 29 and statid.startswith("CLS"): return 27
    if obstyp == 2:
        if varno in [2, 39]: return 10  # AIREP_T
        if varno in [3, 4, 41, 42]: return 11  # AIREP_U
    if obstyp == 3: return 12  # SATOB_U
    if obstyp == 4:
        if varno == 1: return 13  # DRIBU_Z
        if varno in [3, 4, 41, 42]: return 14
    if obstyp == 6:
        if varno == 1: return 15
        if varno in [3, 4, 41, 42]: return 16
    if obstyp == 7:
        if sensor == 3: return 17  # AMSUA
        if sensor in [4, 15]: return 18  # AMSUB/MHS
        if sensor == 19: return 19  # ATMS
        if sensor == 73: return 20  # MWHS2
        if sensor == 16: return 21  # IASI
        if sensor == 27: return 22  # CRIS
        if sensor == 29: return 23  # SEVIRI
    if obstyp == 9: return 24  # SCATT_U
    if obstyp == 13:
        if varno == 29: return 25  # RADAR_Z
        if varno == 195: return 26  # RADAR_U
    if obstyp == 10: return 28  # GPS-RO
    if obstyp == 19: return 29  # SGNSS
    return 0

def read_next(file):
    line = file.readline()
    if not line:
        return None

    parts = shlex.split(line)
    if len(parts) < 12:
        raise ValueError(f"Bad INPUT: Malformed line with {len(parts)} fields: {line}. Expecting 12 columns in ASCII input.")

    try:
        return {
            'obstype@hdr': int(parts[0]),
            'codetype@hdr': int(parts[1]),
            'vertco_reference_1@body': float(parts[2]),
            'sensor@hdr': int(parts[3]),
            'statid@hdr': parts[4].strip(),
            'varno@body': int(parts[5]),
            'lat@hdr': float(parts[6]),
            'lon@hdr': float(parts[7]),
            'obsvalue@body': float(parts[8]),
            'final_obs_error@errstat': float(parts[9]),
            'fg_depar@body': float(parts[10]),
            'an_depar@body': float(parts[11])
        }
    except ValueError as e:
        print(f"Error parsing line: {line}")
        raise e

def plot_results(mode="percent"):
    dfsobsstr = (
        'SYNOP-Z','SYNOP-T2','SYNOP-R2','SYNOP-U10','GNSS-ZTD',
        'TEMP-U','TEMP-T','TEMP-Z','TEMP-Q','AIREP-T',
        'AIREP-U','SATOB-U','BUOY-Z','BUOY-U','PILOT-Z',
        'PILOT-U','AMSUA-TB','MHS-TB','ATMS-TB','MWHS2-TB',
        'IASI-TB','CRIS-TB','SEVIRI-TB','SCATT-U','RADAR-Z',
        'RADAR-U','TEMP_CLS','GPS-RO','SGNSS'
    )

    values = []
    labels = []

    total_dfs = np.sum(valdfs)

    for i in range(numvar):
        dfs = valdfs[i]
        nobs = numdfs[i]
        if dfs == 0:
            continue  # skip zeros

        if mode == "raw":
            val = dfs
        elif mode == "perobs":
            val = dfs / nobs if nobs > 0 else 0
        elif mode == "percent":
            val = (dfs / total_dfs) * 100 if total_dfs > 0 else 0
        else:
            print(f"Unknown plot mode: {mode}")
            return

        values.append(val)
        labels.append(dfsobsstr[i])

    plt.figure(figsize=(12, 8))
    plt.barh(labels, values)
    plt.xlabel({ "raw": "Total DFS []", "perobs": "DFS per Observation []", "percent": "Total DFS [%]" }[mode])
    plt.title({ "raw": "Absolute DFS by Observation Type", "perobs": "Relative DFS (per observation) by Observation Type", "percent": "DFS Percentage Contribution by Observation Type" }[mode])
    plt.gca().invert_yaxis()
    plt.grid(True, axis='x', linestyle='--', alpha=0.6)

    ax = plt.gca()
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    plt.tight_layout()
    filename = f"dfs_plot_{mode}.png"
    plt.savefig(filename)
    print(f"datool_dfs: Bar chart saved to {filename}")

def main():
    global valdfs, numdfs, cntdfs, cntnot

    args = parse_args()

    # List plot styles available
    if args.list_plot_styles:
        print("datool_dfs: Available matplotlib plot styles:")
        for style in plt.style.available:
            print(f"  {style}")
        sys.exit(0)

    # Apply the selected plot style
    try:
        plt.style.use(args.plot_style)
    except OSError:
        print(f"Error: '{args.plot_style}' is not a valid matplotlib style.")
        print("Use --list-plot-styles to see available options.")
        sys.exit(1)

    # Validate required files unless just listing styles
    if not args.file1 or not args.file2:
        parser.error("the following arguments are required: file1, file2 (unless using --list-plot-styles)")

    file1 = args.file1
    file2 = args.file2

    type1_odb = is_odb_file(file1)
    type2_odb = is_odb_file(file2)
    type1_ascii = is_ascii_file(file1)
    type2_ascii = is_ascii_file(file2)

    if type1_odb and type2_odb:
        print("datool_dfs: Detected ODB input files.")
        records1 = read_odb_file(file1)
        records2 = read_odb_file(file2)
    elif type1_ascii and type2_ascii:
        print("datool_dfs: Detected ASCII input files.")
        f1 = open(file1)
        f2 = open(file2)
        f1.readline()
        f2.readline()
        records1 = []
        records2 = []
        while True:
            rec1 = read_next(f1)
            rec2 = read_next(f2)
            if rec1 is None or rec2 is None:
                break
            records1.append(rec1)
            records2.append(rec2)
        f1.close()
        f2.close()
    else:
        print("Error: Input files must both be ODB or ASCII. Mismatch or unknown format.")
        sys.exit(1)

    if len(records1) != len(records2):
        print("Error: Input files have different number of records.")
        sys.exit(1)

    print(f"datool_dfs: Options OK. Let's process data ...")
    for rec1, rec2 in zip(records1, records2):

        mismatch = (
            rec1['lon@hdr'] != rec2['lon@hdr'] or
            rec1['lat@hdr'] != rec2['lat@hdr'] or
            rec1['obstype@hdr']  != rec2['obstype@hdr'] or
            rec1['codetype@hdr']  != rec2['codetype@hdr'] or
            rec1['varno@body']  != rec2['varno@body'] or
            rec1['final_obs_error@errstat'] != rec2['final_obs_error@errstat']
        )
        if mismatch:
            print("datool_dfs: Inconsistency between input files, skip this observation.")
            continue

        iind = determine_index(rec1['obstype@hdr'], rec1['varno@body'], rec1['codetype@hdr'], rec1['sensor@hdr'], int(round(rec1['vertco_reference_1@body'])), rec1['statid@hdr'])
        if iind == 0:
            cntnot += 1
            print("datool_dfs: Unclassified:", rec1)
            continue

        diff_obs = rec1['obsvalue@body'] - rec2['obsvalue@body']
        diff_dep = (rec1['fg_depar@body'] - rec1['an_depar@body']) - (rec2['fg_depar@body'] - rec2['an_depar@body'])
        weight = 1.0 / (rec1['final_obs_error@errstat'] ** 2)
        valdfs[iind - 1] += abs(diff_obs * weight * diff_dep)
        numdfs[iind - 1] += 1

    cntdfs = np.sum(numdfs)

    print(f"datool_dfs: Observations used   : {cntdfs}")
    print(f"datool_dfs: Observations unused : {cntnot}")

    if args.write_dfs:
        with open("dfs.dat", "w") as out:
            for i in range(numvar):
                out.write(f"{i+1:6d}{numdfs[i]:7d}{valdfs[i]:10.3f}\n")
        print("datool_dfs: Raw DFS data written to dfs.dat")

    if args.plot:
        plot_results(args.plot)

if __name__ == "__main__":
    main()

