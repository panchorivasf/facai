import sys, csv, os
from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib import colors
from reportlab.platypus import (SimpleDocTemplate, Paragraph, Spacer, Table,
                                  TableStyle, HRFlowable, Image, KeepTogether,
                                  PageBreak)

tmp_dir  = sys.argv[1]
out_path = sys.argv[2]

def read_csv(name):
    with open(f"{tmp_dir}/{name}") as f:
        return list(csv.DictReader(f))

meta    = {r["key"]: r["value"] for r in read_csv("meta.csv")}
status  = read_csv("status.csv")
flags   = read_csv("flags.csv")
dbh     = read_csv("dbh.csv")
growth  = read_csv("growth.csv")

# Plot metadata is optional — only loaded when the file exists
plot_meta_path = os.path.join(tmp_dir, "plot_metadata.csv")
plot_meta = read_csv("plot_metadata.csv") if os.path.exists(plot_meta_path) else None

doc    = SimpleDocTemplate(out_path, pagesize=letter,
                            leftMargin=72, rightMargin=72,
                            topMargin=72, bottomMargin=72,
                            title = "GFB3 Report")
styles = getSampleStyleSheet()

# ── Styles ───────────────────────────────────────────────────────────────────
h1 = ParagraphStyle("h1", parent=styles["Heading1"],
                    fontSize=16, textColor=colors.HexColor("#2E4057"),
                    spaceAfter=6)
h2 = ParagraphStyle("h2", parent=styles["Heading2"],
                    fontSize=12, textColor=colors.HexColor("#2E4057"),
                    spaceBefore=14, spaceAfter=4)
body  = styles["Normal"]
small = ParagraphStyle("small", parent=styles["Normal"], fontSize=8)

def tbl_style(header_color="#2E4057"):
    return TableStyle([
        ("BACKGROUND",     (0, 0), (-1, 0), colors.HexColor(header_color)),
        ("TEXTCOLOR",      (0, 0), (-1, 0), colors.white),
        ("FONTNAME",       (0, 0), (-1, 0), "Helvetica-Bold"),
        ("FONTSIZE",       (0, 0), (-1, -1), 9),
        ("ROWBACKGROUNDS", (0, 1), (-1, -1),
         [colors.HexColor("#F5F5F5"), colors.white]),
        ("GRID",           (0, 0), (-1, -1), 0.4, colors.HexColor("#CCCCCC")),
        ("LEFTPADDING",    (0, 0), (-1, -1), 6),
        ("RIGHTPADDING",   (0, 0), (-1, -1), 6),
        ("TOPPADDING",     (0, 0), (-1, -1), 4),
        ("BOTTOMPADDING",  (0, 0), (-1, -1), 4),
    ])

story = []

# ── Title ────────────────────────────────────────────────────────────────────
story.append(Paragraph("GFB3 Format Diagnostic Report", h1))
story.append(Paragraph(f"Dataset: {meta['dataset_name']}", body))
story.append(HRFlowable(width="100%", thickness=1,
                         color=colors.HexColor("#2E4057")))
story.append(Spacer(1, 12))

# ── Overview ─────────────────────────────────────────────────────────────────
# Pull country / site info from plot_meta when available for a richer overview
if plot_meta:
    countries   = sorted(set(r["Country"] for r in plot_meta if r.get("Country")))
    sites       = sorted(set(r["Site"]    for r in plot_meta if r.get("Site")))
    country_str = ", ".join(countries) if countries else "—"
    site_str    = ", ".join(sites)     if sites    else "—"
    # When we have plot metadata the country/site replaces the generic "N plot(s)" phrase
    overview_text = (
        f"This report summarises the diagnostic results for the <b>{meta['dataset_name']}</b> "
        f"dataset formatted to GFB3 standard. The dataset contains <b>{int(meta['n_rows']):,}</b> "
        f"rows representing <b>{int(meta['n_trees']):,}</b> individual stems censused between "
        f"<b>{meta['yr_min']}</b> and <b>{meta['yr_max']}</b>, across "
        f"<b>{meta['n_plots']}</b> plot(s) in <b>{country_str}</b> (site: {site_str})."
    )
else:
    overview_text = (
        f"This report summarises the diagnostic results for the <b>{meta['dataset_name']}</b> "
        f"dataset formatted to GFB3 standard. The dataset contains <b>{int(meta['n_rows']):,}</b> "
        f"rows across <b>{meta['n_plots']}</b> plot(s), representing "
        f"<b>{int(meta['n_trees']):,}</b> individual stems censused between "
        f"<b>{meta['yr_min']}</b> and <b>{meta['yr_max']}</b>."
    )
story.append(KeepTogether([
    Paragraph("Overview", h2),
    Paragraph(overview_text, body),
    Spacer(1, 10),
]))

# ── Plot Locations (only when plot_metadata was supplied) ────────────────────
if plot_meta:
    # Decide which columns to show and their display widths.
    # Long columns (PlotID, Site) get more space; coords get less.
    display_cols   = ["Country", "Site", "PlotID", "PI", "Censuses",
                      "Size", "Latitude", "Longitude"]
    col_widths_map = {
        "Country":   52,
        "Site":      80,
        "PlotID":    90,
        "PI":        80,
        "PIe":       90,
        "Censuses":  52,
        "Size":      36,
        "Latitude":  52,
        "Longitude": 52,
    }
    # Keep only columns that actually exist in the data
    display_cols = [c for c in display_cols if c in (plot_meta[0].keys())]
    col_widths   = [col_widths_map.get(c, 60) for c in display_cols]

    pm_data = [display_cols] + [
        [r.get(c, "") for c in display_cols] for r in plot_meta
    ]
    pm_tbl = Table(pm_data, colWidths=col_widths, splitByRow=True)
    pm_tbl.setStyle(tbl_style())

    map_path = os.path.join(tmp_dir, "plot_map.png")
    pm_block = [
        Paragraph("Plot Locations", h2),
        Paragraph(
            "The table below lists the unique plots included in this dataset. "
            "Coordinates are in decimal degrees (WGS84). "
            "Size is in hectares; Censuses reports the number of inventories "
            "per plot.", body),
        Spacer(1, 6),
        pm_tbl,
    ]
    if os.path.exists(map_path):
        pm_block += [
            Spacer(1, 10),
            Paragraph(
                "The map below shows the geographic distribution of the plots.", body),
            Spacer(1, 6),
            Image(map_path, width=450, height=321),
        ]
    pm_block.append(Spacer(1, 10))
    story.append(KeepTogether(pm_block))

# ── Status Distribution ───────────────────────────────────────────────────────
s_data = [["Status", "Label", "Count", "Pct (%)"]] + [
    [r["Status"], r["Label"], f"{int(r['n']):,}", r["pct"]] for r in status
]
s_tbl = Table(s_data, colWidths=[50, 180, 70, 70], splitByRow=False)
s_tbl.setStyle(tbl_style())

story.append(KeepTogether([
    Paragraph("Status Distribution", h2),
    Paragraph(
        "The table below shows the breakdown of tree records by GFB3 status code. "
        "Status 0 (alive) should dominate; elevated counts of status 9 (missing) "
        "may indicate census gaps.", body),
    Spacer(1, 6),
    s_tbl,
    Spacer(1, 10),
]))

# ── DBH Summary ──────────────────────────────────────────────────────────────
d_keys = list(dbh[0].keys())
d_data = [d_keys] + [[r[k] for k in d_keys] for r in dbh]
d_tbl  = Table(d_data, splitByRow=False)
d_tbl.setStyle(tbl_style())

dbh_hist_path = os.path.join(tmp_dir, "dbh_hist.png")
dbh_block = [
    Paragraph("DBH Summary (cm)", h2),
    Paragraph(
        "Descriptive statistics for diameter at breast height (DBH, cm). "
        "Values below 10 cm fall below the GFB3 minimum threshold and should "
        "be reviewed.", body),
    Spacer(1, 6),
    d_tbl,
]
if os.path.exists(dbh_hist_path):
    dbh_block += [Spacer(1, 8), Image(dbh_hist_path, width=400, height=267)]
dbh_block.append(Spacer(1, 10))
story.append(KeepTogether(dbh_block))

# ── Growth Summary ────────────────────────────────────────────────────────────
g_keys = list(growth[0].keys())
g_data = [g_keys] + [[r[k] for k in g_keys] for r in growth]
g_tbl  = Table(g_data, splitByRow=False)
g_tbl.setStyle(tbl_style())

growth_hist_path = os.path.join(tmp_dir, "growth_hist.png")
growth_block = [
    Paragraph("Growth Summary (cm DBH / interval)", h2),
    Paragraph(
        "Summary of per-interval DBH increments. Negative values indicate apparent "
        "shrinkage, which may reflect measurement error or POM changes. Values "
        "exceeding 5 cm/yr warrant individual inspection.", body),
    Spacer(1, 6),
    g_tbl,
]
if os.path.exists(growth_hist_path):
    growth_block += [Spacer(1, 8), Image(growth_hist_path, width=400, height=267)]
growth_block.append(Spacer(1, 10))
story.append(KeepTogether(growth_block))

# ── Data Quality Flags ────────────────────────────────────────────────────────
sev_colors = {"critical": "#FFDDC1", "warning": "#FFF9C4", "info": "#E8F5E9"}
f_data = [["Flag", "Count", "Severity"]] + [
    [r["Flag"], r["Count"], r["Severity"]] for r in flags
]
f_tbl = Table(f_data, colWidths=[270, 70, 80], splitByRow=False)
base_style = tbl_style()
for i, r in enumerate(flags, start=1):
    bg = sev_colors.get(r["Severity"], "#FFFFFF")
    base_style.add("BACKGROUND", (0, i), (-1, i), colors.HexColor(bg))
f_tbl.setStyle(base_style)

story.append(KeepTogether([
    Paragraph("Data Quality Flags", h2),
    Paragraph(
        "Each flag below reports a count of records triggering a specific quality "
        "check. Critical flags must be resolved; warnings are worth reviewing but "
        "may be acceptable depending on the dataset.", body),
    Spacer(1, 6),
    f_tbl,
    Spacer(1, 14),
]))

# ── Verdict ───────────────────────────────────────────────────────────────────
verdict = meta["verdict"]
vcolor  = ("#C62828" if verdict.startswith("FAIL")
           else "#F57F17" if verdict.startswith("WARN")
           else "#2E7D32")

story.append(KeepTogether([
    HRFlowable(width="100%", thickness=1, color=colors.HexColor("#2E4057")),
    Spacer(1, 6),
    Paragraph("Verdict", h2),
    Paragraph(f'<font color="{vcolor}"><b>{verdict}</b></font>', body),
]))

# ── Curation Log ─────────────────────────────────────────────────────────────
notes_path = os.path.join(tmp_dir, "curation_log.txt")
if os.path.exists(notes_path):
    with open(notes_path, "r", encoding="utf-8") as f:
        notes_text = f.read().strip()

    if notes_text:
        notes_paras = [Paragraph("Curation Log", h2)]
        for line in notes_text.splitlines():
            if line.strip():
                notes_paras.append(Paragraph(line, body))
            else:
                notes_paras.append(Spacer(1, 4))
        notes_paras.append(Spacer(1, 10))
        story.append(KeepTogether(notes_paras))

doc.build(story)