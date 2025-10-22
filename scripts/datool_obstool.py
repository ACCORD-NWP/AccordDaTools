#!/usr/bin/env python3

import sys
import argparse
import shlex
import numpy as np
import matplotlib.pyplot as plt

import pyodc as odc



#python script.py --file1 dataDTG1.odb  --file2 dataDTG2.odb gpsdel
#datool_obstool --file1=gpsdel_2021091200 --file2=gpsdel_2021091203 --plot gpsdel
def parse_args():
    parser = argparse.ArgumentParser(
        description="Read the input file and the type of obs you want to plot.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument('--file1', help='ODB-2/ASCII file from first CCMA')
    parser.add_argument('--file2', help='ODB-2/ASCII file from second CCMA')
#convlist="ship_z airep_t airep_u airep_v radar_RH radar_DOW gpsdel ascat"
#satlist="amsua mhs iasi seviri mwhs2 atms"
    parser.add_argument('obstype', choices=['ship_z','airep_t','airep_u','airep_v','radar_RH','radar_DOW','gpsdel','ascat'],
                        help="Obs:\n"
                             "  ship_z )\n"
                             "  airep_t )\n"
                             "  airep_u )\n"
                             "  airep_v )\n"
                             "  radar_RH )\n"
                             "  radar_DOW )\n"
                             "  gpsdel )\n"
                             "  ascat"
    )
    parser.add_argument('--plot-style', help='Matplotlib style for plotting', default='default')
    parser.add_argument('--list-plot-styles', action='store_true', help='List available matplotlib plot styles and exit')

    # Check if no arguments at all
    if len(sys.argv) == 1:
        print(f"datool_obstool: Error: No arguments provided.")
        parser.print_usage()
        sys.exit(1)

    return parser.parse_args()

    print(parser.parse_args)
    print("Observation to plot", obs)
    print("Period",file1, file2)

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

    print(obstyp)
    print(varno)


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
#This part to prepare the ODB2 general file and select the VAR and write a ascii file with the six columns: satid","lat","lon","an_depar","fg_depar","obsvalue" 

'''
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
    #convlist="ship_z airep_t airep_u airep_v radar_RH radar_DOW gpsdel ascat"
    #plt.xlabel({ "ship_z": "ship_z", "airep_t": "airep_t", "airep_u": "airep_u","airep_v": "airep_v","radar_RH": "radar_RH","radar_DOW": "radar_DOW","gpsdel": "gpsdel", "ascat": "ascat" }[obs])
    plt.title({ "ship_z": "ship_z", "airep_t": "airep_t", "airep_u": "airep_u","airep_v": "airep_v","radar_RH": "radar_RH","radar_DOW": "radar_DOW","gpsdel": "gpsdel", "ascat": "ascat" }[obs])

    #plt.xlabel({ "raw": "Total DFS []", "perobs": "DFS per Observation []", "percent": "Total DFS [%]" }[mode])
    #plt.title({ "raw": "Absolute DFS by Observation Type", "perobs": "Relative DFS (per observation) by Observation Type", "percent": "DFS Percentage Contribution by Observation Type" }[mode])
    plt.gca().invert_yaxis()
    plt.grid(True, axis='x', linestyle='--', alpha=0.6)

    ax = plt.gca()
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    plt.tight_layout()
    filename = f"dfs_plot_{mode}.png"
    plt.savefig(filename)
    print(f"datool_dfs: Bar chart saved to {filename}")
'''

def main():

    CDTG1='2021091200' #this would come from the name of file1
    CDTG2='2021091203' #this would come from the name of file1
    DIRIN='/home/mdy/test_tools/da_tools'
    DIROUT='/home/mdy/test_tools/da_tools'
    VAR='gpsdel' #this would come from the command line
    exe1='preparing_conv.R'
    exe2='monitoring_conv'
    EXP='expname'

    #this would be firts part 
    # R CMD BATCH  "--args ${CDTG1} ${DIRIN} ${DIROUT} ${VAR}" ${exe1} > ${DIROUT}/prepare_${VAR}.out
    # R CMD BATCH  "--args ${CDTG2} ${DIRIN} ${DIROUT} ${VAR}" ${exe1} > ${DIROUT}/prepare_${VAR}.out

   #this would be the second part
    #R CMD BATCH  "--args ${EXP} ${DIROUT} ${SDTG} ${EDTG} ${CYCLE} ${VAR}" ${exe2} > ${DIROUT}/monitor_${VAR}.out

    #    global valdfs, numdfs, cntdfs, cntnot
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
        print("datool_obstool: Detected ODB input files.")
        records1 = read_odb_file(file1)
        records2 = read_odb_file(file2)
    elif type1_ascii and type2_ascii:
        print("datool_obstool: Detected ASCII input files.")
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

    print(f"datool_obstool: Options OK. Let's process data ...")

    if args.write_obstool:
        with open("obstool.dat", "w") as out:
            for i in range(numvar):
                out.write(f"{i+1:6d}{numdfs[i]:7d}{valdfs[i]:10.3f}\n")
        print("datool_obstool: Raw Obstooldata written to obstool.dat")

    if args.plot:
        plot_results(args.plot)

if __name__ == "__main__":
    main()


