#' Extract and Compile Zipped Sensorgnome Data
#'
#' This functions compiles the zipped folders in a Sensorgnome data package and compiles them into a .txt file.
#' @param folder_path the path to the extracted folder with the Sensorgnome data. It should look like: "SG-1AAARPI3A1AB-2026-07-01T14_20_55.123Z/".
#' @param output_file the desired name of the output file. Don't forget to include the ".txt" extension at the end.


extract_sg <- function(folder_path, output_file) {


  if (endsWith(output_file,".txt") == FALSE) {
    stop("Please specify a .txt file as your output_file")
  }

  if (endsWith(folder_path,"/") == FALSE) {
    stop("Please include "/" at the end of your folder path")
  }

# Starting point and output file
############# Need to extract the zipped file
### Just right click and click on "Extract All"

# Function to process files recursively
process_files_recursive <- function(folder_path) {
  # List all files and directories in the current folder
  files <- list.files(folder_path, full.names = TRUE)

  # Filter for directories
  directories <- files[file.info(files)$isdir]

  # Filter for .txt.gz files
  txt_gz_files <- files[grepl("\\.txt\\.gz$", files)]

  # Process .txt.gz files
  for (file_path in txt_gz_files) {
    gz_con <- gzfile(file_path, "rt")  # Open connection to gzipped file
    file_content <- readLines(gz_con)  # Read content using readLines
    close(gz_con)  # Close the connection

    file_name <- tools::file_path_sans_ext(basename(file_path))  # Extract file name without extension

    output_file_path <- file.path(folder_path, paste0(file_name, ".txt"))  # Define output file path

    writeLines(file_content, output_file_path)  # Write content to plain text file
  }

  # Recursively process subdirectories
  for (dir_path in directories) {
    process_files_recursive(dir_path)
  }
}


# Call the recursive function to process all files
process_files_recursive(folder_path)


##############################################################################

# Function to combine all text files recursively
combine_txt_files <- function(folder_path, output_file) {
  # List all files and directories in the current folder
  files <- list.files(folder_path, full.names = TRUE, recursive = TRUE)

  # Filter for .txt files
  txt_files <- files[grepl("\\.txt$", files)]

  # Initialize an empty character vector to store combined content
  combined_content <- character()

  # Process each .txt file
  for (file_path in txt_files) {
    file_content <- readLines(file_path, warn = FALSE)  # Read content of the file
    combined_content <- c(combined_content, file_content)  # Append content to combined_content
  }

  # Write combined content to the output file
  writeLines(combined_content, output_file)
}


# Call function to combine all text files
combine_txt_files(folder_path, output_file)
}
