# JReduce

JReduce is a tool for reducing Java class files and jar files.

## Installation

JReduce is most easily installed with
[stack](https://docs.haskellstack.org/en/stable/README/).

```
stack install
```

## Usage

JReduce can reduce Java files given a predicate. Simple usage is best
described with `jreduce [...] -o OUTPUT INPUT CMD [ARG..]`. The CMD is
any program that test the property of the input file, often it is a bash
script. You can add any number of arguments to the command:

```
jreduce -o ouput.jar input.jar ./predicate.sh %anotherfile.txt {} 
```

JReduce will now try to produce the smallest jar-file that satisfies the
predicate. The predicate will be run in separated workspaces so that
they do not interfere with each other. In this case the `{}` will be
substituted with the unpacked and reduced folder of classes, and 
the `%` prefix means that the input is a file. Like this:

```
/abs/path/to/predicate.sh /abs/path/to/anotherfile.txt reduced_input/
```

We can describe the criteria of success by using `--stdout`, `--stderr`,
or `--exit-code ?`. If `--stdout`, and `--stder` is on the reducer will
keep the output the same.

There are a lot of extra options which can be explored by running
`jreduce -h`:

```
jreduce

Usage: jreduce [...] INPUT CMD [ARG..]
  A command line tool for reducing java programs.

Available options:
  INPUT                    the path to the jar or folder to reduce.
  -v                       make it more verbose.
  -q                       make it more quiet.
  -D,--log-depth ARG       set the log depth. (default: -1)
  -c,--core CORE           the core classes to not reduce. Can add a file of
                           classes by prepending @.
  --cp CLASSPATH           the library classpath of things not reduced. This is
                           useful if the core files is not in the reduction,
                           like when you are reducing a library using a
                           test-suite
  --stdlib                 load the standard library? This is unnecessary for
                           most reductions.
  --jre JRE                the location of the stdlib.
  -o,--output FILE         set the output path.
  -r,--recursive           remove other files and reduce internal jars.
  -S,--strategy STRATEGY   reduce by class instead of by closure (default:
                           closure).Choose between closure, reject-item, and
                           item.
  -R,--reducer ARG         the reducing algorithm to use. (default: Binary)
  -E,--exit-code CODE      preserve exit-code (default: 0)
  --stdout                 preserve stdout.
  --stderr                 preserve stderr.
  --total-time SECS        the maximum seconds to run all predicates, negative
                           means no timelimit. (default: -1.0)
  --max-iterations ITERS   the maximum number of time to run the predicate,
                           negative means no limit. (default: -1)
  -W,--work-folder ARG     the work folder.
  -K,--keep-folders        keep the work folders after use?
  --metrics ARG            the metrics output, defaults to metric.csv in the
                           work folder.
  -T,--timelimit SECS      the maximum number of seconds to run the process,
                           negative means no timelimit. (default: -1.0)
  CMD                      the command to run
  ARG..                    arguments to the command.
  -h,--help                Show this help text
```
