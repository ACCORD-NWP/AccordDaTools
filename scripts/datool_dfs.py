#!/usr/bin/env python3

import sys
import argparse
import shlex
import numpy as np
import matplotlib.pyplot as plt

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
    parser.add_argument('--file1', help='ODB ASCII file from perturbed CCMA')
    parser.add_argument('--file2', help='ODB ASCII file from unperturbed CCMA')
    parser.add_argument('--write-dfs', action='store_true', help='Write raw DFS data to dfs.dat')
    parser.add_argument('--plot', nargs='?', const='percent', choices=['raw', 'perobs', 'percent'],
        help="Generate a DFS plot:\n"
             "  percent - percentage contribution (default)"
             "  perobs  - DFS per observation\n"
             "  raw     - total DFS\n"
    )
    parser.add_argument('--plot-style', help='Matplotlib style for plotting', default='default')
    parser.add_argument('--list-plot-styles', action='store_true', help='List available matplotlib plot styles and exit')

    return parser.parse_args()

def determine_index(indtyp, indvar, ios, indsen, ichan, chst):
    # Return observation type index (1-based)
    if indtyp == 1:
        if indvar == 1: return 1   # SYNOP_Z
        if indvar == 39: return 2  # SYNOP_T2
        if indvar in [58, 40]: return 3  # SYNOP_R2
        if indvar in [41, 42]: return 4  # SYNOP_U10
        if indvar == 128: return 5  # GNSS_ZTD
    if indtyp == 5:
        if indvar in [3, 4, 41, 42]: return 6  # TEMP_U
        if indvar in [2, 39]: return 7  # TEMP_T
        if indvar == 1: return 8  # TEMP_Z
        if indvar in [58, 59, 40, 7]: return 9  # TEMP_Q
        if indvar == 29 and chst.startswith("CLS"): return 27
    if indtyp == 2:
        if indvar in [2, 39]: return 10  # AIREP_T
        if indvar in [3, 4, 41, 42]: return 11  # AIREP_U
    if indtyp == 3: return 12  # SATOB_U
    if indtyp == 4:
        if indvar == 1: return 13  # DRIBU_Z
        if indvar in [3, 4, 41, 42]: return 14
    if indtyp == 6:
        if indvar == 1: return 15
        if indvar in [3, 4, 41, 42]: return 16
    if indtyp == 7:
        if indsen == 3: return 17  # AMSUA
        if indsen in [4, 15]: return 18  # AMSUB/MHS
        if indsen == 19: return 19  # ATMS
        if indsen == 73: return 20  # MWHS2
        if indsen == 16: return 21  # IASI
        if indsen == 27: return 22  # CRIS
        if indsen == 29: return 23  # SEVIRI
    if indtyp == 9: return 24  # SCATT_U
    if indtyp == 13:
        if indvar == 29: return 25  # RADAR_Z
        if indvar == 195: return 26  # RADAR_U
    if indtyp == 10: return 28  # GPS-RO
    if indtyp == 19: return 29  # SGNSS
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
            'indtyp': int(parts[0]),
            'ios': int(parts[1]),
            'zch': float(parts[2]),
            'indsen': int(parts[3]),
            'chst': parts[4].strip(),
            'indvar': int(parts[5]),
            'zlat': float(parts[6]),
            'zlon': float(parts[7]),
            'zobs': float(parts[8]),
            'zerr': float(parts[9]),
            'fgdep': float(parts[10]),
            'andep': float(parts[11])
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
    print(f"dfscomp: Bar chart saved to {filename}")

def main():
    global valdfs, numdfs, cntdfs, cntnot

    args = parse_args()

    if args.list_plot_styles:
        print("Available matplotlib plot styles:")
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

    try:
        f1 = open(file1)
        f2 = open(file2)
    except FileNotFoundError:
        usage()

    f1.readline()
    f2.readline()

    print(f"dfscomp: Options OK. Let's process data ...")
    while True:
        rec1 = read_next(f1)
        rec2 = read_next(f2)
        if rec1 is None or rec2 is None:
            break

        mismatch = (
            rec1['zlon'] != rec2['zlon'] or
            rec1['zlat'] != rec2['zlat'] or
            rec1['indtyp']  != rec2['indtyp'] or
            rec1['ios']  != rec2['ios'] or
            rec1['indvar']  != rec2['indvar'] or
            rec1['zerr'] != rec2['zerr']
        )
        if mismatch:
            print("Inconsistency between input files, skip this observation.")
            continue

        iind = determine_index(rec1['indtyp'], rec1['indvar'], rec1['ios'], rec1['indsen'], int(round(rec1['zch'])), rec1['chst'])
        if iind == 0:
            cntnot += 1
            print("Unclassified:", rec1)
            continue

        diff_obs = rec1['zobs'] - rec2['zobs']
        diff_dep = (rec1['fgdep'] - rec1['andep']) - (rec2['fgdep'] - rec2['andep'])
        weight = 1.0 / (rec1['zerr'] ** 2)
        valdfs[iind - 1] += abs(diff_obs * weight * diff_dep)
        numdfs[iind - 1] += 1

    f1.close()
    f2.close()

    cntdfs = np.sum(numdfs)

    print(f"dfscomp: Observations used   : {cntdfs}")
    print(f"dfscomp: Observations unused : {cntnot}")

    if args.write_dfs:
        with open("dfs.dat", "w") as out:
            for i in range(numvar):
                out.write(f"{i+1:6d}{numdfs[i]:7d}{valdfs[i]:10.3f}\n")
        print("Raw DFS data written to dfs.dat")

    if args.plot:
        plot_results(args.plot)

if __name__ == "__main__":
    main()

