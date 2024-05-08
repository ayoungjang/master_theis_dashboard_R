import subprocess
import seaborn as sns
from faicons import icon_svg
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Import data from shared.py
from shared import app_dir, df

from shiny import reactive
from shiny.express import input, render, ui
from shiny.types import FileInfo

FILE_PATH = "./Rscript/disk/index.R"

ui.page_opts(title="MIC Dashboard", fillable=True)

with ui.sidebar(title="Filter controls"):
    ui.input_file("data","Data", accept=[".xlsx"], multiple=False)
    ui.input_file("reference","Reference", accept=[".xlsx"], multiple=False)
    ui.input_action_button("execute","Execute")  # Button to execute plot

with ui.layout_columns():
    @render.table
    @reactive.event(input.execute)
    def summary():
        data_path, reference_path = parsed_file()  # Get paths from the parsed_file function

        # Debugging output to check paths
        print("Data Path:", data_path)
        print("Reference Path:", reference_path)

        if data_path and reference_path:
            # Call the R script with paths as arguments
            try:
                result = subprocess.run(
                    ["/usr/bin/Rscript", "./Rscript/disk/index.R", data_path, reference_path],
                    capture_output=True, text=True
                )

                print("here1")
                print("R Script Output:", result.stdout)
                print("R Script Error:", result.stderr)
                print("here2")
            except Exception as e:
                print("Failed to run R script:", e)
        else:
            print("File paths are not available.")


@reactive.calc
def parsed_file():
    data_file_info: list[FileInfo] | None = input.data()  # Assuming 'data' is the ID for the data file input
    reference_file_info: list[FileInfo] | None = input.reference()  # Assuming 'reference' is the ID for the reference file input
    data_path = data_file_info[0]['datapath'] if data_file_info else None
    reference_path = reference_file_info[0]['datapath'] if reference_file_info else None
    return data_path, reference_path

ui.include_css("styles.css")  # Make sure this path is correct
