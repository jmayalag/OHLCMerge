library("argparse")

parser <- ArgumentParser(description = "Merge files in directory")

parser$add_argument("dirs",
  metavar = "DIR", type = "character", nargs = "+",
  help = "merge files in one or more directories"
)

parser$add_argument("-o", "--output",
  help = "output directory (default=.)",
  action = "store",
  default = "."
)

parser$add_argument("-r", "--recursive",
  help = "recursively search files in DIR",
  action = "store_true"
)

parser$add_argument("--save_summary",
  help = "save summary as csv",
  action = "store_true"
)

parser$add_argument("-v", "--verbose",
  help = "recursively search files",
  action = "store_true"
)

library(OHLCMerge)

files <- dir(args$dirs, pattern = "*.csv", full.names = TRUE, recursive = args$recursive)
grouped <- auto_group_files(files)

summary <- merge_files_by_group(grouped,
  save_dir = args$output,
  save_summary = args$save_summary,
  log = args$verbose, info = args$verbose, verbose = args$verbose
)

print(summary)
