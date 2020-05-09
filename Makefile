.PHONY: clean

clean:
	find . -maxdepth 1 -type f \( -name '*.class' -or -name '*.o' -or -name '*.nasm' \) -delete
	find e2e/ -maxdepth 1 -type f \( -name '*.class' -or -name '*.o' -or -name '*.nasm' \) -delete
	find . -maxdepth 1 -type f -perm +111 -delete
	find e2e/ -maxdepth 1 -type f -perm +111 -delete
	find . -type d -name META-INF | xargs rm -rf
