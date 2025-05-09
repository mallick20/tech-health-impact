
# title: "Process all 2002-2019 files"
# output: html_notebook


library(data.table)

# Define input file path
input_file <- "health_data/raw/NSDUH-2002-2019-DS0001-bndl-data-tsv/NSDUH_2002_2019_Tab.tsv"

# Open a connection to read in chunks
chunk_size <- 1000  # Adjust based on your system's memory
finished <- FALSE
skip_lines <- 0

# Read header to get column names
header <- fread(input_file, nrows = 0)

num_lines_processed <- 0


while (!finished) {
  # Read chunk
  chunk <- tryCatch({
    fread(input_file, skip = skip_lines, nrows = chunk_size, header = FALSE, col.names = names(header), sep = "\t")
  }, error = function(e) NULL)
  
  if (is.null(chunk) || nrow(chunk) == 0) {
    finished <- TRUE
    break
  }
  
  # Process each year in this chunk
  years_in_chunk <- unique(chunk$YEAR)
  
  for (yr in years_in_chunk) {
    year_data <- chunk[chunk$YEAR == yr]
    cat(yr)
    
    output_file <- paste0("../health_data/raw/NSDUH_", yr, "_Tab.txt")
    
    # If file exists, append without header
    if (file.exists(output_file)) {
      fwrite(year_data, output_file, append = TRUE, sep="\t")
    } else {
      # Create new file with header
      fwrite(year_data, output_file, sep="\t")
    }
  }
  
  # Update skip_lines for next chunk
  skip_lines <- skip_lines + nrow(chunk)
  
  cat("Processed", skip_lines, "rows...\n")
}
