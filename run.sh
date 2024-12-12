docker run -i dockerfile

# example usage of console IO

cat .\test\simple_features.md  | docker run -i dockerfile run-c

# example usage of file IO (edit makefile to run files that you want)
# current files:
#            in: simple_feature.md
#            out: output.html

docker run -i dockerfile run
