## How to use PCT

To use PCT on a test, you need to know the length of the test. The depth is empirically set to 5.
You can profile that by running the test with basicpos for a short repeat. e.g.

```
./run.py --repeat 100 --sched basicpos -c locks-1
```

Then feed the output to `summarize.py`
