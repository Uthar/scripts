# Steps for generating Nix expressions for ASDF systems:
#
## URI
#
# You need to provide an SQLite file containing the following table:
#   create table if not exists project (name unique, uri)
#
# The name is the name of the project containing ASDF systems.
#
# The URI is a fully qualified link to the project source.
#
## Fetch
#
# Receives an SQLite file as input, reads project/URI pairs from a table.
# Attempts to realise the project source, choosing a method based on URI host.
# Provides an API for navigating the source as a filesystem.
# (use builtins.fetchTree?)
#
## FindAsd
#
# Traverses project sources looking for .asd system definition files.
# Writes discovered asds to SQLite.
#
## LoadAsdForMeta
#
# Attempts to load ASD files to discover their metadata.
# The metadata contains dependencies, version, licence, and systems contained in
# the .asd files.
# If a system is missing because of defsystem-depends-on, adds it to
# dependencies and tries again.
# Writes the metadata to SQLite
#
## CreateAsds
#
# Ensures that every non-slashy system exists in its own .asd file.
# Creates such a file if it does not exist.
# (Ensures that each system source only contains a single .asd file.)
#
# 

import os
import sqlite3
import subprocess
import json
import traceback
from urllib.parse import urlencode

db = None

def list_project_names():
    "Get all project names"
    return os.listdir("projects")

def write_project(project):
    "Write project name and source information to SQLite"
    db.execute("insert or replace into proj values (?,?)", project)

def read_source_txt(project_name):
    "Read project's source.txt file"
    with open(os.path.join("projects", project_name, "source.txt")) as f:
        source = f.readline()
        return source.strip()

def parse_branched_git(url, branch, *_):
    return f"git+{url}?ref={branch}"

def parse_ediware_http(url, *_):
    return parse_git(f"https://github.com/edicl/{url}")

def parse_git(url, *_):
    return f"git+{url}"

def parse_http(url, *_):
    return url

def parse_https(url, *_):
    return url

def parse_kmr_git(url, *_):
    return parse_git(f"http://git.kpe.io/{url}")

def parse_latest_github_release(url, *_):
    return parse_git(url)

def parse_latest_github_tag(url, *_):
    return parse_git(url)

def parse_latest_gitlab_release(url, *_):
    return parse_git(url)

def parse_darcs(url, *_):
    return f"darcs+{url}"

def parse_svn(url, *_):
    return f"svn+{url}"

def parse_mercurial(url, *_):
    return f"hg+{url}"

def parse_single_file(url, *_):
    return url

def parse_tagged_git(url, tag, *_):
    return f"git+{url}?tag={tag}"

def get_parser(kind):
    "Return a function to parse Quicklisp project source based on its kind"
    try:
        return globals()["parse_" + kind.replace("-","_").lower()]
    except KeyError:
        raise NotImplementedError(f"Quicklisp {kind} projects are not supported")

def parse_source(source):
    "Parse Quicklisp source line into a canonical URI"
    kind, url, *args = source.split()
    parse = get_parser(kind) 
    return parse(url, *args)

def parse_project(project_name):
    return (project_name, parse_source(read_source_txt(project_name)))

def write_all_projects():
    for project_name in list_project_names():
        try:
            write_project(parse_project(project_name))
        except Exception as e:
            print(f"Failed to parse {project_name}: {e}")
            pass

def shell_command_to_string(command, timeout=None):
    "Return the stdout of shell command as string"
    return subprocess.check_output(command, timeout=timeout)

def fetch_tree(url):
    "Call Nix to use builtins.fetchTree and return the resulting attrs"
    try:
        attrs = shell_command_to_string([
            'nix', 'eval', '--impure', '--expr', f'builtins.fetchTree {url}'
        ], 20)
    except subprocess.CalledProcessError as e:
        raise NotImplementedError(f"Fetching of url {url} failed: {e}")
    # Nix prints only the outPath attr if it exists, preventing the --json flag
    attrs = attrs.decode('utf-8').replace("outPath =", "path =")
    as_json = shell_command_to_string(['nix', 'eval', '--json', '--expr', attrs])
    return json.loads(as_json)

def process_all_projects(f):
    with db:
        for row in db.execute("select * from proj"):
            try:
                f(row)
            except Exception as e:
                print(f"Processing {row} failed: {e}")

def fetch(project):
    "Download a project's source. Return a filesystem path."
    name, url = project
    with db:
        def check_path():
            query = ["select meta -> 'path' from fetch where url=?", [url]]
            return next(db.execute(*query), None)
        def check_error():
            query = ["select msg from error where key=?", [url]]
            return next(db.execute(*query), None)
        if not (check_path() or check_error()):
            print(f"re-fetching {url}")
            try:
                meta = json.dumps(fetch_tree(url))
                db.execute("insert or replace into fetch values (?,?)", [url,meta])
            except Exception as e:
                error = traceback.format_exc()
                db.execute("insert or replace into error values (?,?)", [url,error])
    return project

def discover_asds(project):
    name, url = project
    with db:
        res = db.execute("select meta -> 'path' from fetch where url=?", [url])
        path = next(res)[0].strip('"')
    asds = set()
    for root, dirs, files in os.walk(path):
        for file in files:
            _, ext = os.path.splitext(file)
            if ext == ".asd":
                asds.add(file)
    asds = json.dumps(list(asds))
    with db:
        db.execute("insert or replace into asd values (?,?)", [name, asds])
    return project

def init_db():
    db.execute("create table if not exists proj (project unique, url not null)")
    db.execute("create table if not exists fetch (url unique, meta not null)")
    db.execute("create table if not exists asd (project unique, asd not null)")
    db.execute("create table if not exists meta (system unique, meta not null)")
    db.execute("create table if not exists error (key unique, msg not null)")

# TODO:
# - save errors to db
# - strip " from names
# - handle git+git uri

def main():
    global db
    db = sqlite3.connect("projects.db")
    init_db()
    try:
        write_all_projects()
        process_all_projects(lambda project: discover_asds(fetch(project)))
    finally:
        db.close()
    
if __name__ == "__main__":
    main()
