import streamlit as st
import os
import re
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
from PIL import Image

# Set page configuration to wide so we can custom-build our central wrapper
st.set_page_config(layout="wide")

# Directory containing your PNG plots
png_dir = "./Figures/PNG/Animated"

if not os.path.exists(png_dir):
    st.error(f"PNG directory not found: {png_dir}")
    st.stop()

# ==========================================
# 1. PARSING FILENAMES & MAPPING DICTIONARY
# ==========================================
def parse_three_way_cut(filename):
    """
    Extracts the two cut points from filenames like 'Animated_MA2026_10_50.png'
    Returns a tuple of integers: (10, 50)
    """
    match = re.search(r"Animated_MA2026_(\d+)_(\d+)\.png", filename)
    return (int(match.group(1)), int(match.group(2))) if match else None

def build_png_dict(directory):
    """Scans directory and builds a map of (cut1, cut2) -> filename"""
    return {
        cuts: f for f in os.listdir(directory)
        if f.startswith("Animated_MA2026_") and f.endswith(".png")
        for cuts in [parse_three_way_cut(f)]
        if cuts
    }

png_files = build_png_dict(png_dir)

# ==========================================
# 2. GLOBAL PAGE LAYOUT WRAPPER
# ==========================================
# Generates side margins: [Left Padding, App Body, Right Padding]
# Squeezes your entire application presentation into the middle 50% of the display.
col_blank_left, col_main, col_blank_right = st.columns([1, 2, 1])

with col_main:
    # ------------------------------------------
    # HEADER SECTION
    # ------------------------------------------
    st.header("Interactive Supplementary Figure 11")
    st.subheader("PRS Three-Tier Stratification Dashboard")
    st.markdown("---")

    # ------------------------------------------
    # 3. INTERACTIVE LAYOUT ROW (PLOT + SLIDERS)
    # ------------------------------------------
    col_dist, col_sliders = st.columns([1.2, 1])

    with col_sliders:
        st.markdown("##### Select Distribution Cut Points")        
        # First slider defines the boundary between Low and Mid
        cut1 = st.slider("First Cut Point (Low to Mid %)", min_value=10, max_value=50, step=10, value=10)
        
        # Second slider defines the boundary between Mid and High
        cut2 = st.slider("Second Cut Point (Mid to High %)", min_value=cut1, max_value=90, step=10, value=90)

    selected_cuts = (cut1, cut2)

    with col_dist:
        x = np.linspace(-4, 4, 1000)
        y = norm.pdf(x, 0, 1)

        # Convert the percentile cut points into statistical Z-scores
        z_cut1 = norm.ppf(cut1 / 100)
        z_cut2 = norm.ppf(cut2 / 100)

        fig, ax = plt.subplots(figsize=(4, 2.5))
        ax.plot(x, y, color="black", linewidth=1.5)
        
        # Shade Tier 1 (Low): 0 to cut1%
        ax.fill_between(x, y, where=(x <= z_cut1), color="#F8766D", alpha=0.5, 
                        label=f"Low (0–{cut1}%)")
        
        # Shade Tier 2 (Mid): cut1% to cut2%
        ax.fill_between(x, y, where=((x > z_cut1) & (x <= z_cut2)), color="#00CC00", alpha=0.5, 
                        label=f"Mid ({cut1}–{cut2}%)")
        
        # Shade Tier 3 (High): cut2% to 100%
        ax.fill_between(x, y, where=(x > z_cut2), color="#619CFF", alpha=0.5, 
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

    # ------------------------------------------
    # 4. RENDER OUTCOME PLOT (BOTTOM)
    # ------------------------------------------
    st.subheader("Viewing Plots between Selected Stratification")

    if selected_cuts in png_files:
        target_png = os.path.join(png_dir, png_files[selected_cuts])
        
        img = Image.open(target_png)
        st.image(img, use_container_width=True, caption=f"PRS Tier Results: 0-{cut1}% | {cut1}-{cut2}% | {cut2}-100%")
        #st.info(f"Viewing File: `{png_files[selected_cuts]}`")
    else:
        st.warning(f"No figure available for this specific configuration.")
