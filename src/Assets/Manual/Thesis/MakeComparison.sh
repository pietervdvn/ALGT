PANDOC_EXTENSIONS="--tab-stop=8 -f markdown+raw_tex+link_attributes+grid_tables+pandoc_title_block+pipe_tables+implicit_header_references --listings"

pandoc $PANDOC_EXTENSIONS 2.2FeatComparison.md -o ../.bin/Comparison.tex

