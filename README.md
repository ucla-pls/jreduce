# JReduce

JReduce is a tool for reducing Java class files and jar files.

## Installation

JReduce can be installed using
[stack](https://docs.haskellstack.org/en/stable/README/).

```
stack install
```

Or using nix:

```
nix-env -if .
```

## Usage

JReduce can reduce Java files given a predicate. Simple usage is best
described with `jreduce [...] -o OUTPUT INPUT -- CMD [ARG..]`. The CMD is
any program that test the property of the input file, often it is a bash
script. You can add any number of arguments to the command:

```
jreduce -o ouput.jar input.jar -- ./predicate.sh %anotherfile.txt {} 
```

JReduce will now try to produce the smallest jar-file that satisfies the
predicate. The predicate will be run in separated workspaces so that
they do not interfere with each other. In this case the `{}` will be
substituted with the unpacked and reduced folder of classes, and 
the `%` prefix means that the input is a file. This is important, because the 
script is not rexecuted from this folder.

We can describe the criteria of success by using `--preserve`.  If you are
using `--preserve out,exit` it will preserve both the exit code and the
standard output.

There are a lot of extra options which can be explored by running
`jreduce -h`:

```
jreduce

Usage: jreduce INPUT [-R|--reducer RED] CMD [ARG..]
  A command line tool for reducing java programs.

Available options:
  -S,--strategy STRATEGY   reduce by different granularity (default:
                           items+logic).Choose between classes, items+logic, and
                           items+graph+first/last.
  INPUT                    the path to the jar or folder to reduce.
  -v                       make it more verbose.
  -q                       make it more quiet.
  -D,--log-depth ARG       set the log depth. (default: -1)
  -c,--core CORE           the core classes to not reduce. Can add a file of
                           classes by prefixing @.
  --cp CLASSPATH           the library classpath of things not reduced. This is
                           useful if the core files is not in the reduction,
                           like when you are reducing a library using a
                           test-suite
  --stdlib ARG             load and save the stdlib to this stubsfile. Choose
                           between .json and .bin formats.
  --jre JRE                the location of the stdlib.
  --dump                   dump all to the workfolder.
  --dump-graph             dump graph to the workfolder.
  --dump-closures          dump closures to the workfolder.
  --dump-core              dump core to the workfolder.
  --dump-items             dump item terms to the workfolder.
  --dump-logic             dump the logical statement to the workfolder.
  --dump-stubs             dump the hierachy stubs.
  -o,--output-file OUTPUT  specifies where to put the output
  -W,--work-folder ARG     the work folder.
  -f,--force               delete the work folder if it already exists.
  --keep-hierarchy         do not reduce the class hierarchy
  --reverse-order          reverse the order of the variables
  -R,--reducer RED         the reducing algorithm to use. Choose from ddmin,
                           linear, or binary. (default: "binary")
  --total-time SECS        the maximum seconds to run all predicates, negative
                           means no timelimit. (default: -1.0)
  --max-iterations ITERS   the maximum number of time to run the predicate,
                           negative means no limit. (default: -1)
  --keep-folders           keep the reduction folders after use?
  --keep-outputs           keep the stdout and stderr outputs?
  --metrics-file ARG       an absolute or relative (to the WORKFOLDER/reduction)
                           path of the metric file (default: "metrics.csv")
  --predicate-timelimit ARG
                           the timelimit of the predicate in seconds, negative
                           means no limit (default: -1.0)
  --try-initial            try the intitial problem, recored as (0000)
  --ignore-failure         ignore the failure of the initial try.
  -p,--preserve ARG        a comma separated list of what to preserve. Choose
                           from out, err, and exit. (default: "exit")
  --exit ARG               require a specific exit code.
  --out ARG                require a specific stdout file.
  --err ARG                require a specific stderr file.
  -T,--timelimit SECS      the maximum number of seconds to run the process,
                           negative means no timelimit. (default: -1.0)
  CMD                      the command to run
  ARG..                    arguments to the command.
  -h,--help                Show this help text
```

## Example 

Consider the java program in the `example/` folder.

```java
// A.java
public class A implements I {
    public String m () {
        return "Hello, Alice!";
    };
    public B n () {
        return null;
    };
}

// B.java
public class B implements I {
    public String m () {
        return "Hello, Bob!"; 
    };
    public B n () {
        return null;
    };
}

// I.java
public interface I {
    public String m ();
    public B n ();
}

// Main.java
public class Main {
  public String x (I a) { return a.m(); }

  public static void main (String [] args) {
    System.out.println(new Main().x(new A()));
  }
}
```

Try to compile and run the program (Tested using Java 1.8):

```
$ javac *.java -d classes/
$ java -cp classes/ Main
Hello, Alice!
```

The complied program can be inspected using javap:

```java
// $ javap classes/*
Compiled from "A.java"
public class A implements I {
  public A();
  public java.lang.String m();
  public B n();
}
Compiled from "B.java"
public class B implements I {
  public B();
  public java.lang.String m();
  public B n();
}
Compiled from "I.java"
public interface I {
  public abstract java.lang.String m();
  public abstract B n();
}
Compiled from "Main.java"
public class Main {
  public Main();
  public java.lang.String x(I);
  public static void main(java.lang.String[]);
}
```

Let's imagine that we want to reduce the program while preserving the output.
We can simply run jreduce like this:

```
$ jreduce classes/ -p out -- java -cp {} Main
16:27:21 ├ JReduce
16:27:21 │ ├ Started JReduce.
16:27:21 │ ├ Reading inputs
16:27:21 │ │ └ 0.003s
16:27:21 │ ├ Calculating Initial Problem
16:27:21 │ │ ├ setup
16:27:21 │ │ │ └ 0.002s
16:27:21 │ │ ├ run
16:27:21 │ │ │ └ 0.180s
16:27:21 │ │ └ 0.182s
16:27:21 │ ├ Initializing key function
16:27:21 │ │ ├ Calculating the hierarchy
16:27:21 │ │ │ ├ Load stdlib stubs
16:27:28 │ │ │ │ └ 6.463s
...
```

After which we can run the command again:

```
$ java -cp classes/ Main
Hello, Alice!
```

And we can see that the behaviour has been preserved while the classes have been reduced:

```java
// $ javap classes/*
public class A implements I {
  public A();
  public java.lang.String m();
}
public interface I {
  public abstract java.lang.String m();
}
public class Main {
  public Main();
  public java.lang.String x(I);
  public static void main(java.lang.String[]);
}
```

### Core

It is possible to designate a core of required variables.
This happens by giving items to preserve to the `--core` command.
A full list of items (with indicies) can be aquired by running the jreduce command with `-W workfolder --dump-items`.
Here is the results from the example:

```
-- in workfolder/items.txt after a run 
-- with `jreduce classes/ -W work-folder --dump-items -- java -cp {} Main`
meta[]
A[0]
A.<init>:()V[0,0]
A.<init>:()V!code[0,0,0]
A.m:()Ljava/lang/String;[0,1]
A.m:()Ljava/lang/String;!code[0,1,0]
A.n:()LB;[0,2]
A.n:()LB;!code[0,2,0]
A<I]I[0,3]
B[1]
B.<init>:()V[1,0]
B.<init>:()V!code[1,0,0]
B.m:()Ljava/lang/String;[1,1]
B.m:()Ljava/lang/String;!code[1,1,0]
B.n:()LB;[1,2]
B.n:()LB;!code[1,2,0]
B<I]I[1,3]
I[2]
I.m:()Ljava/lang/String;[2,0]
I.n:()LB;[2,1]
Main[3]
Main.<init>:()V[3,0]
Main.<init>:()V!code[3,0,0]
Main.x:(LI;)Ljava/lang/String;[3,1]
Main.x:(LI;)Ljava/lang/String;!code[3,1,0]
Main.main:([Ljava/lang/String;)V[3,2]
Main.main:([Ljava/lang/String;)V!code[3,2,0]
```

For example we can get jreduce to preserve the method m in B by running the command like this:

```
jreduce classes/ -p out --core 'B.m:()Ljava/lang/String;' -- java -cp {} Main
17:26:19 ├ JReduce
17:26:19 │ ├ Started JReduce.
17:26:19 │ ├ Reading inputs
17:26:19 │ │ └ 0.000s
17:26:19 │ ├ Calculating Initial Problem
17:26:19 │ │ ├ setup
17:26:19 │ │ │ └ 0.001s
17:26:19 │ │ ├ run
17:26:19 │ │ │ └ 0.077s
17:26:19 │ │ └ 0.079s
17:26:19 │ ├ Initializing key function
...
```

Now we also preserve the method in B.

```
$ javap classes/*
public class A implements I {
  public A();
  public java.lang.String m();
}
public class B {
  public java.lang.String m();
}
public interface I {
  public abstract java.lang.String m();
}
public class Main {
  public Main();
  public java.lang.String x(I);
  public static void main(java.lang.String[]);
}
```

J-Reduce also accepts line-seperated cores from files. 
You can write `--core @myfile.txt`.
