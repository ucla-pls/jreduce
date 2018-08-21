#!/usr/bin/env python3

import os
import csv
import sys
import subprocess
from multiprocessing import Pool
from subprocess import DEVNULL, call, check_call, Popen, PIPE, STDOUT, check_output, TimeoutExpired
from shutil import rmtree
from pathlib import Path

import logging

logger = logging.getLogger('reduceall')
logger.setLevel(logging.DEBUG)

ch = logging.StreamHandler(sys.stderr)
ch.setLevel(logging.DEBUG)
# create formatter and add it to the handlers
formatter = logging.Formatter('(%(asctime)s)-%(processName)s: %(message)s')
ch.setFormatter(formatter)
logger.addHandler(ch)

info = logger.info
debug = logger.debug

reductors = [ "ddmin:graph", "gbired", "ddmin:verify", "ddmin:graph-sort"]

def run_jreduce(reductor, classes, work_dir, prop, max_iterations=1000):
    info("Running {}".format(work_dir))
    try:
        call(
            ["jreduce", "-v",
            "--cp", str(classes),
            "-p", "class",
            "-r", reductor,
            "--work-dir", str(work_dir),
            "-m", str(max_iterations),
            ] + prop, stdout=DEVNULL, stderr=DEVNULL, timeout=3600)
    except Exception as e:
        info("{}: {}".format(e, work_dir))
    info("Done running {}".format(work_dir))

def run_property(prop, output_folder):
    stdout = output_folder / "stdout.log"
    with open(str(stdout), "wb") as out:
        p = Popen(["bash", str(prop.resolve()), "classes"],
                  stderr=STDOUT, stdout=PIPE, cwd=str(output_folder))
        for line in p.stdout:
            out.write(line)
        n = p.wait()
        if n != 0:
            info("Property did not succeed; return code = {}".format(n))
        else:
            info("Property succeded, see stdout: {}".format(stdout))

def compute_reduction(jar, prop, output_folder, pool):
    classes = output_folder / "classes"
   
    if not classes.exists():
        classes.mkdir(parents=True)
        if not check_call(["unzip", str(jar.resolve())], stderr=DEVNULL, stdout=DEVNULL, cwd=str(classes)) == 0:
            info("Could not unzip {}, continuing..".format(jar.resolve()))
            return

    if not (output_folder / "stdout.log").exists():
        pool.apply_async(run_property, (prop, output_folder))

    for red in reductors:
        folder = output_folder / red
        if not folder.exists():
            pool.apply_async(run_jreduce, (red, classes, folder, [str(prop)]))

def analyse_jar(jar, prop, basefolder, pool):
    name = jar.stem
    prop_name = prop.stem
    output_folder = basefolder / prop_name / name

    compute_reduction(jar, prop, output_folder, pool)

    dct = { "name" : name, "prop": prop_name}

    for red in reductors:
        iters = 0
        final = -1
        score = 0
        best_score = 0

        try:
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
        except:
            pass
        
        dct[red + " iter"] = iters
        dct[red + " size"] = final
        dct[red + " change"] = 0 if best_score == 0 else score / best_score

    classes = list((output_folder / "classes").glob("**/*.class"))
    dct["classes"] = len(classes)

    return dct


if __name__ == "__main__":
    cmd, basefolder, *args = sys.argv

    i = args.index("--")
    props = [Path(prop) for prop in args[:i]]
    jars = [Path(jar) for jar in args[i+1:]]

    writer = csv.DictWriter(sys.stdout,
            ["name", "prop", "classes"] +
                            sum([[red + " " + kind for red in reductors ] for  kind in ["iter", "size", "change"]], []))
    writer.writeheader()

    with Pool() as p:
        for jar in jars:
            for prop in props:
                r = analyse_jar(jar, prop, Path(basefolder), p)
                if r:
                    writer.writerow(r)
        p.close()
        p.join()
