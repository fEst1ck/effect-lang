.PHONY: all pdf test clean

# Default target
all: pdf test

# Generate PDF from README.md using pandoc
pdf:
	pandoc README.md -o README.pdf

# Run Racket unit tests
test:
	raco test lang.rkt

# Clean generated files
clean:
	rm -f README.pdf