#!/usr/bin/env python3

import os
import csv
import sys
import subprocess
from multiprocessing import Pool
from subprocess import DEVNULL, call, check_call, Popen, PIPE, STDOUT, check_output
from shutil import rmtree
from pathlib import Path

import logging

logger = logging.getLogger('reduceall')
logger.setLevel(logging.DEBUG)

ch = logging.StreamHandler(sys.stderr)
ch.setLevel(logging.DEBUG)
# create formatter and add it to the handlers
formatter = logging.Formatter('(%(asctime)s): %(message)s')
ch.setFormatter(formatter)
logger.addHandler(ch)

info = logger.info
debug = logger.debug

jreduce_folder = Path(os.path.realpath(__file__)).parent.parent
test_suite = jreduce_folder / "test-suite"
reductors = [ "ddmin", "ddmin:graph", "gbired", "ddmin:verify"]

def run_jreduce(reductor, classes, work_dir, prop, max_iterations=1000):
    call(
        ["jreduce", "-v",
         "--cp", str(classes),
         "-r", reductor,
         "--work-dir", str(work_dir),
         "-m", str(max_iterations),
        ] + prop)


def compute_reduction(jar, prop, output_folder):
    classes = output_folder / "classes"
    classes.mkdir(parents=True)

    if not check_call(["unzip", str(jar.resolve())], stderr=DEVNULL, stdout=DEVNULL, cwd=str(classes)) == 0:
        info("Could not unzip {}, continuing..".format(jar.resolve()))
        return

    stdout = output_folder / "stdout.log"
    with open(str(stdout), "wb") as out:
        p = Popen(["bash", str(prop.resolve()), "classes"], stderr=STDOUT, stdout=PIPE, cwd=str(output_folder))
        for line in p.stdout:
            out.write(line)
        n = p.wait()
        if n != 0:
            info("Property did not succeed; return code = {}".format(n))
            return
        else:
            info("Property succeded, see stdout: {}".format(stdout.relative_to(Path.cwd())))

    info("Running on reductors:")
    for red in reductors:
        info("Running {}".format(red))
        run_jreduce(red, classes, output_folder / red, [str(prop)])

def analyse_jar(jar, prop):
    name = jar.stem
    prop_name = prop.stem
    output_folder = test_suite / prop_name / name

    if output_folder.exists():
        info("Already did {}".format(name))
    else:
        info("Working on {}".format(name))
        compute_reduction(jar, prop, output_folder)



    dct = { "name" : name}

    for red in reductors:
        iters = 0
        final = -1
        score = 0
        best_score = 0


        with open(str(output_folder / red / "progress.csv")) as c:
            for line in csv.DictReader(c):
                if "class-closure-" + red in line["Step"]:
                    iters += 1
                if "after-class-closure" in line["Step"]:
                    final = line["Classes"]
                    diffname = output_folder / red / "after-class-closure.diff"
                    call(
                        "diff --left-column --side-by-side {} {} > {}"
                        .format(output_folder / "stdout.log",
                                output_folder / red / line["Step"] / "stderr.log",
                                diffname),
                        shell=True)
                    with open(str(diffname)) as r:
                        for line in r:
                            best_score +=1
                            if line.endswith("(\n") or line.endswith("<\n"):
                                score +=1

        dct[red + " iter"] = iters
        dct[red + " size"] = final
        dct[red + " change"] = 0 if best_score == 0 else score / best_score

    p = Popen("javaq -f csv --cp {}".format(jar), stdout=PIPE, universal_newlines=True, shell=True)
    classes = list(csv.DictReader(p.stdout))
    p.wait();
    dct["classes"] = len(classes)

    return dct


if __name__ == "__main__":
    cmd, prop, *jars = sys.argv

    writer = csv.DictWriter(sys.stdout,
            ["name", "classes"] +
                            sum([[red + " " + kind for red in reductors ] for  kind in ["iter", "size", "change"]], []))
    writer.writeheader()

    def run(jar):
        return analyse_jar(Path(jar), Path(prop))

    with Pool() as p:
        for row in p.map(run, jars):
            writer.writerow(row)
