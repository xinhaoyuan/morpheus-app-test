The test use /tmp/ra as temporary dir.

`make eunit` would repeat a case for 100 times.

For following shell command will keep running until a error is found:

```
i=0; while (make eunit > /tmp/ra_test_output.txt); do i=$((i + 1)); echo Pass $i finished; done; echo Stop at pass $i
```
