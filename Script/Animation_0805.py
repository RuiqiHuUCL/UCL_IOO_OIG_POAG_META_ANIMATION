# Tried to finalise the code:
# Add the Recommadation?
# Add distribution?
# Add Binary variable

import streamlit as st
from PIL import Image
import os
import re
import numpy as np
import matplotlib.pyplot as plt

# Set the folder containing KM plots
image_dir = "/Users/richie/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/LiGHT_SA/Manuscript/Main//Results_1030/Other_cutoff/both_eye"
# image_dir = '/Users/richied/OneDrive - University College London/Desktop/LiGHT_SA/Manuscript/All_combinations_KM'

bv_image_dir = "/Users/richie/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/LiGHT_SA/Manuscript/Main/Results_1030/Other_cutoff/BV"

if not os.path.exists(image_dir):
    st.error(f"Image directory not found: {image_dir}")
    st.stop()

# Extract high/low from filenames
def parse_high_low(filename):
    match = re.search(r"high(\d+)_low(\d+)", filename)
    return (int(match.group(1)), int(match.group(2))) if match else None

def build_file_dict(prefix, directory):
    return {
        hl: f for f in os.listdir(directory)
        if f.startswith(prefix) and f.endswith(".png")
        for hl in [parse_high_low(f)]
        if hl
    }
    
# Build dictionaries for SLT and Med files
slt_files = build_file_dict("Lymph_SLT", image_dir)
med_files = build_file_dict("Lymph_Med", image_dir)
bv_files = build_file_dict("Lymph_BV", bv_image_dir)

# Two-column layout for distribution plot and sliders
col_dist, col_sliders = st.columns([1, 1])

# Move sliders to col_sliders
high_value = col_sliders.slider("Select High PRS (%)", min_value=10, max_value=90, step=10, value=10)
low_value = col_sliders.slider("Select Low PRS (%)", min_value=10, max_value=90, step=10, value=10)

# Convert percentages to file keys
high_selected = 11 - (high_value // 10)  # 10% -> 10, 20% -> 9, ...
low_selected = low_value // 10           # 10% -> 1, 20% -> 2, ...
selected = (high_selected, low_selected)

high_label = f"Top {high_value}%"
low_label = f"Bottom {low_value}%"

# Plot normal distribution with highlighted high and low thresholds
from scipy.stats import norm
x = np.linspace(-4, 4, 1000)
y = 1/np.sqrt(2*np.pi) * np.exp(-x**2/2)

# Compute cutoff values based on the normal distribution quantiles
high_cutoff = norm.ppf(1 - high_value / 100)
low_cutoff = norm.ppf(low_value / 100)

fig, ax = plt.subplots(figsize=(4, 3))
ax.plot(x, y, color="black")
ax.fill_between(x, y, where=(x >= high_cutoff), color="#F8766D", alpha=0.4, label=f"Top {high_value}%")
ax.fill_between(x, y, where=(x <= low_cutoff), color="#40E0D0", alpha=0.4, label=f"Bottom {low_value}%")
ax.legend()
ax.set_xlabel("PRS distribution")

# Move plot to col_dist
col_dist.pyplot(fig)

# Display SLT and Med side by side
col1, col2 = st.columns(2)

def show_image(col, files, label):
    if selected in files:
        img_path = os.path.join(image_dir, files[selected])
        col.image(Image.open(img_path), caption=label)
    else:
        col.write(f"No plot available for {label}")

with col1:
    show_image(col1, slt_files, f"SLT: {high_label} vs {low_label}")

with col2:
    show_image(col2, med_files, f"Medication: {high_label} vs {low_label}")

st.markdown("---")
st.subheader("PRS recommendation")

if selected in bv_files:
    bv_img_path = os.path.join(bv_image_dir, bv_files[selected])
    st.image(Image.open(bv_img_path), caption=f"BV: {high_label} vs {low_label}")
else:
    st.write(f"No plot available for BV: {high_label} vs {low_label}")
