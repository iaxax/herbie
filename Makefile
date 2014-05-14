all: report

report: 
	racket reports/make-report.rkt "bench/"
	./file-report.sh
clean:
	rm -f index.md
	find reports/* -type d -exec rm -r {} \;

cost:
	$(CC) -O0 cost.c -lm -o cost
