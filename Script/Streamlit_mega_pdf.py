import streamlit as st
import os
import re
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
import base64

# Set page configuration
st.set_page_config(layout="wide")

# Directory containing your PDF plots
pdf_dir = "../Figures/PDF_all/Animated"

if not os.path.exists(pdf_dir):
    st.error(f"PDF directory not found: {pdf_dir}")
    st.stop()

# ==========================================
# 1. PARSING FILENAMES & MAPPING DICTIONARY
# ==========================================
def parse_three_way_cut(filename):
    """
    Extracts the two cut points from filenames like 'Animated_MA2026_10_50.pdf'
    Returns a tuple of integers: (10, 50)
    """
    match = re.search(r"Animated_MA2026_(\d+)_(\d+)\.pdf", filename)
    return (int(match.group(1)), int(match.group(2))) if match else None

def build_pdf_dict(directory):
    """Scans directory and builds a map of (cut1, cut2) -> filename"""
    return {
        cuts: f for f in os.listdir(directory)
        if f.startswith("Animated_MA2026_") and f.endswith(".pdf")
        for cuts in [parse_three_way_cut(f)]
        if cuts
    }

pdf_files = build_pdf_dict(pdf_dir)

# ==========================================
# 2. CONTROL DASHBOARD & SLIDERS
# ==========================================
st.title("PRS Three-Tier Stratification")
st.markdown("---")

col_dist, col_sliders = st.columns([1.2, 1])

with col_sliders:
    st.subheader("Select Distribution Cut Points")
    
    # First slider defines the boundary between Low and Mid (e.g., 10)
    cut1 = st.slider("First Cut Point (Low to Mid %)", min_value=10, max_value=50, step=10, value=10)
    
    # Second slider defines the boundary between Mid and High (e.g., 50)
    # Forced to always be higher than cut1
    cut2 = st.slider("Second Cut Point (Mid to High %)", min_value=cut1, max_value=90, step=10, value=50)

selected_cuts = (cut1, cut2)

# ==========================================
# 3. THREE-TIER DISTRIBUTION PLOT
# ==========================================
with col_dist:
    x = np.linspace(-4, 4, 1000)
    y = norm.pdf(x, 0, 1)

    # Convert the percentile cut points into statistical Z-scores
    z_cut1 = norm.ppf(cut1 / 100)
    z_cut2 = norm.ppf(cut2 / 100)

    fig, ax = plt.subplots(figsize=(5, 3))
    ax.plot(x, y, color="black", linewidth=1.5)
    
    # Shade Tier 1 (Low): 0 to cut1%
    ax.fill_between(x, y, where=(x <= z_cut1), color="#40E0D0", alpha=0.5, 
                    label=f"Low (0–{cut1}%)")
    
    # Shade Tier 2 (Mid): cut1% to cut2%
    ax.fill_between(x, y, where=((x > z_cut1) & (x <= z_cut2)), color="#FFB000", alpha=0.5, 
                    label=f"Mid ({cut1}–{cut2}%)")
    
    # Shade Tier 3 (High): cut2% to 100%
    ax.fill_between(x, y, where=(x > z_cut2), color="#F8766D", alpha=0.5, 
                    label=f"High ({cut2}–100%)")
    
    # Visual styling details
    ax.legend(loc="upper right", fontsize='small')
    ax.set_xlabel("PRS Distribution Tiers", fontsize=9)
    ax.set_ylabel("Density", fontsize=9)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    plt.tight_layout()
    
    st.pyplot(fig)

st.markdown("---")

# ==========================================
# 4. RENDER EMBEDDED PDF
# ==========================================
st.subheader("Analysis Document Viewing")

def display_pdf(pdf_path):
    """Reads a local PDF file and encodes it in base64 to embed into an HTML iframe."""
    with open(pdf_path, "rb") as f:
        base64_pdf = base64.b64encode(f.read()).decode('utf-8')
    pdf_display = f'<iframe src="data:application/pdf;base64,{base64_pdf}" width="100%" height="800px" type="application/pdf"></iframe>'
    st.markdown(pdf_display, unsafe_allow_html=True)

if selected_cuts in pdf_files:
    target_pdf = os.path.join(pdf_dir, pdf_files[selected_cuts])
    st.info(f"Viewing File: `{pdf_files[selected_cuts]}`")
    display_pdf(target_pdf)
else:
    st.warning(f"No generated PDF available for this specific configuration. (Expected filename: `Animated_MA2026_{cut1}_{cut2}.pdf`)")