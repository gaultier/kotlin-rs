.PHONY: clean

clean:
	find . -path ./target -prune -o -type f \( -name '*.class' -or -name '*.o' -or -name '*.nasm' -or -name '*.exe' \) -delete
