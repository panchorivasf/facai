import sys, csv
from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib import colors
from reportlab.platypus import (SimpleDocTemplate, Paragraph, Spacer, Table,
                                  TableStyle, HRFlowable)

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

doc    = SimpleDocTemplate(out_path, pagesize=letter,
                            leftMargin=72, rightMargin=72,
                            topMargin=72, bottomMargin=72)
styles = getSampleStyleSheet()

# ── Styles ───────────────────────────────────────────────────────────────────
h1 = ParagraphStyle("h1", parent=styles["Heading1"],
                    fontSize=16, textColor=colors.HexColor("#2E4057"),
                    spaceAfter=6)
h2 = ParagraphStyle("h2", parent=styles["Heading2"],
                    fontSize=12, textColor=colors.HexColor("#2E4057"),
                    spaceBefore=14, spaceAfter=4)
body = styles["Normal"]

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
story.append(Paragraph("Overview", h2))
overview_text = (
    f"This report summarises the diagnostic results for the <b>{meta['dataset_name']}</b> "
    f"dataset formatted to GFB3 standard. The dataset contains <b>{int(meta['n_rows']):,}</b> "
    f"rows across <b>{meta['n_plots']}</b> plot(s), representing "
    f"<b>{int(meta['n_trees']):,}</b> individual stems censused between "
    f"<b>{meta['yr_min']}</b> and <b>{meta['yr_max']}</b>."
)
story.append(Paragraph(overview_text, body))
story.append(Spacer(1, 10))

# ── Status Distribution ───────────────────────────────────────────────────────
story.append(Paragraph("Status Distribution", h2))
story.append(Paragraph(
    "The table below shows the breakdown of tree records by GFB3 status code. "
    "Status 0 (alive) should dominate; elevated counts of status 9 (missing) "
    "may indicate census gaps.", body))
story.append(Spacer(1, 6))

s_data = [["Status", "Label", "Count", "Pct (%)"]] + [
    [r["Status"], r["Label"], f"{int(r['n']):,}", r["pct"]] for r in status
]
s_tbl = Table(s_data, colWidths=[50, 180, 70, 70])
s_tbl.setStyle(tbl_style())
story.append(s_tbl)
story.append(Spacer(1, 10))

# ── DBH Summary ──────────────────────────────────────────────────────────────
story.append(Paragraph("DBH Summary (cm)", h2))
story.append(Paragraph(
    "Descriptive statistics for diameter at breast height (DBH, cm). "
    "Values below 10 cm fall below the GFB3 minimum threshold and should "
    "be reviewed.", body))
story.append(Spacer(1, 6))

d_keys = list(dbh[0].keys())
d_data = [d_keys] + [[r[k] for k in d_keys] for r in dbh]
d_tbl  = Table(d_data)
d_tbl.setStyle(tbl_style())
story.append(d_tbl)
story.append(Spacer(1, 10))

# ── Growth Summary ────────────────────────────────────────────────────────────
story.append(Paragraph("Growth Summary (cm DBH / interval)", h2))
story.append(Paragraph(
    "Summary of per-interval DBH increments. Negative values indicate apparent "
    "shrinkage, which may reflect measurement error or POM changes. Values "
    "exceeding 5 cm/yr warrant individual inspection.", body))
story.append(Spacer(1, 6))

g_keys = list(growth[0].keys())
g_data = [g_keys] + [[r[k] for k in g_keys] for r in growth]
g_tbl  = Table(g_data)
g_tbl.setStyle(tbl_style())
story.append(g_tbl)
story.append(Spacer(1, 10))

# ── Data Quality Flags ────────────────────────────────────────────────────────
story.append(Paragraph("Data Quality Flags", h2))
story.append(Paragraph(
    "Each flag below reports a count of records triggering a specific quality "
    "check. Critical flags must be resolved; warnings are worth reviewing but "
    "may be acceptable depending on the dataset.", body))
story.append(Spacer(1, 6))

sev_colors = {"critical": "#FFDDC1", "warning": "#FFF9C4", "info": "#E8F5E9"}
f_data = [["Flag", "Count", "Severity"]] + [
    [r["Flag"], r["Count"], r["Severity"]] for r in flags
]
f_tbl = Table(f_data, colWidths=[270, 70, 80])
base_style = tbl_style()
for i, r in enumerate(flags, start=1):
    bg = sev_colors.get(r["Severity"], "#FFFFFF")
    base_style.add("BACKGROUND", (0, i), (-1, i), colors.HexColor(bg))
f_tbl.setStyle(base_style)
story.append(f_tbl)
story.append(Spacer(1, 14))

# ── Verdict ───────────────────────────────────────────────────────────────────
story.append(HRFlowable(width="100%", thickness=1,
                         color=colors.HexColor("#2E4057")))
story.append(Spacer(1, 6))
story.append(Paragraph("Verdict", h2))
verdict = meta["verdict"]
vcolor  = ("#C62828" if verdict.startswith("FAIL")
           else "#F57F17" if verdict.startswith("WARN")
           else "#2E7D32")
story.append(Paragraph(f'<font color="{vcolor}"><b>{verdict}</b></font>', body))

doc.build(story)
