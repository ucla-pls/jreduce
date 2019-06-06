# JReduce

JReduce is a tool for reducing Java class files and jar files.


## Usage

```
jreduce

Usage: jreduce [-v] [-q] [-D|--log-depth ARG] [-c|--core CORE] [--cp CLASSPATH]
               [--stdlib] [--jre JRE] (-t|--target FILE) [-o|--output FILE]
               [-r|--recursive] [-S|--strategy ARG] [-R|--reducer ARG]
               [-E|--exit-code CODE] [--stdout] [--stderr] [--total-time SECS]
               [--max-iterations ITERS] [-W|--work-folder ARG]
               [-K|--keep-folders] [--metrics ARG] [-T|--timelimit SECS] CMD
               [ARG..]
  A command line tool for reducing java programs.

Available options:
  -v                       make it more verbose.
  -q                       make it more quiet.
  -D,--log-depth ARG       set the log depth. (default: -1)
  -c,--core CORE           the core classes to not reduce.
  --cp CLASSPATH           the library classpath, of things not reduced.
  --stdlib                 load the standard library.
  --jre JRE                the location of the stdlib.
  -t,--target FILE         the path to the jar or folder to reduce.
  -o,--output FILE         the path output folder.
  -r,--recursive           remove other files and reduce internal jars.
  -S,--strategy ARG        reduce by class instead of by
                           closure. (default: byclosure)
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
