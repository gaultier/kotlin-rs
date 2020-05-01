.PHONY: clean

clean:
	find . -maxdepth 1 -type f \( -name '*.class' -or -name '*.o' -or -name '*.nasm' \) -delete
	find e2e/ -maxdepth 1 -type f \( -name '*.class' -or -name '*.o' -or -name '*.nasm' \) -delete
