
<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MS-PL License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/sqldf">
    <img src="https://lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

<h3 align="center">SQLDF</h3>

<p align="center">SQLDF is a system for SQL on data frames, optimised for memory.
	<br />
    <a href="https://lisp-stat.dev/docs/subsets/#sql"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/sqldf/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/sqldf/issues">Request Feature</a>
    ·
    <a href="https://lisp-stat.github.io/sqldf/">Reference Manual</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
	<li><a href="#resources">Resources</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About the Project

SQLDF make it easy to query data frames. SQL is the de-facto standard
for data manipulation, with ample learning resources and SQL skills
are ubiquitous.  Although any query that can be done in SQL can also
be done with the data frame API, SQL will be easier for most data
scientists starting out with Lisp Stat.  It is similar to the [R
package of the same
name](https://cran.r-project.org/web/packages/sqldf/index.html).

The price for convenient queries is memory. At worst twice the data
set memory is used: one for the original data frame, and one for the
in-memory SQLite database that is constructed to query.  SQLDF
automatically creates this in-memory database, creates the tables and
schema corresponding to the data frame, transfers the data and
performs the query. The query results are returned as another data
frame. This is suprisingly fast and memory is typically not an issue
on data workstations.


### Built With

* [cl-sqlite](https://github.com/TeMPOraL/cl-sqlite)
* [data-frame](https://github.com/Lisp-Stat/data-frame)
* [select](https://github.com/lisp-stat/select)

<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these steps:

### Prerequisites

An ANSI Common Lisp implementation. Developed and tested with
[SBCL](https://www.sbcl.org/) and
[CCL](https://github.com/Clozure/ccl).

#### Installation

To make the system accessible to [ASDF](https://common-lisp.net/project/asdf/) (a build facility, similar to `make` in the C world), clone the repository in a directory ASDF knows about.  By default the `common-lisp` directory in your home directory is known. Create this if it doesn't already exist and then:

1. Clone the repositories
```sh
cd ~/common-lisp && \
git clone https://github.com/Lisp-Stat/data-frame.git && \
git clone https://github.com/Lisp-Stat/sql-df && \
git clone https://github.com/Lisp-Stat/select && \
git clone https://github.com/TeMPOraL/cl-sqlite.git
```

2. From the REPL reset the ASDF source-registry to find the new systems:
   ```lisp
   (asdf:clear-source-registry)
   ```
3. Load the system
   ```lisp
   (asdf:load-system :sql-df)
   ```

If you have installed the slime ASDF extensions, you can invoke this
with a comma (',') from the slime REPL.

#### Getting dependencies

To get the third party systems that these system may depend on, you can use a dependency manager, such as [Quicklisp](https://www.quicklisp.org/beta/) or [CLPM](https://www.clpm.dev/) Once installed, get the dependencies with either of:

```lisp
(clpm-client:sync :sources "clpi") ;sources may vary
```

```lisp
(ql:quickload :sql-df)
```

You need do this only once. After obtaining the dependencies, you can
load the system with `ASDF` as described above without first syncing
sources.


<!-- USAGE EXAMPLES -->
## Usage

Load the iris data set from R:

```lisp
(asdf:load-system :lisp-stat)
(asdf:load-system :lisp-stat/rdata)
(defdf iris
    (read-csv 'rdata:iris)))
```

and query it:

```lisp
(pprint (sqldf:sqldf "select species, count(*) from iris group by species"))

;;   SPECIES    COUNT(*)
;; 0 setosa           50
;; 1 versicolor       50
;; 2 virginica        50
```

For more examples, please refer to the [Documentation](https://lisp-stat.dev/docs/manuals/sqldf).


<!-- ROADMAP -->
## Roadmap

SQLDF is currently written using an apparently abandoned library,
[cl-sqlite](https://github.com/TeMPOraL/cl-sqlite).  Pull requests
from 2012 have been made with no response from the author, and the
SQLite FFI interface has improved considerably in the 12 years since
it was last updated.

We choose CL-SQLite because, at the time of writing, it was the only
SQLite library with a commercially acceptable license. Since then,
[CLSQL](https://www.cliki.net/CLSQL) has migrated to a BSD license and
is a better option for new development. Not only does it support
[CommonSQL](http://www.lispworks.com/documentation/sql-tutorial/), the
de-facto SQL query syntax for Common Lisp, it also supports several
additional databases.

All new development on SQLDF will be on CLSQL, possibly including some
of the [CSV](https://www.sqlite.org/csv.html) and other extensions
available in SQLite.  Benchmarks show that SQLite's CSV import is
about 15x faster than
[cl-csv](https://github.com/AccelerationNet/cl-csv), and a FFI
wrapper of SQLite's CSV importer would be a good addition to
Lisp-Stat.

Also see the [open issues](https://github.com/lisp-stat/sqldf/issues) for a list of proposed features (and known issues).

## Resources

This system is part of the [Lisp-Stat](https://lisp-stat.dev/) project; that should be your first stop for information. Also see the <!-- [resources](https://lisp-stat.dev/resources) and -->
[community](https://lisp-stat.dev/community) page for more
information.

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are greatly appreciated.  Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details on the code of conduct, and the process for submitting pull requests.

<!-- LICENSE -->
## License

Distributed under the MS-PL License. See [LICENSE](LICENSE) for more information.



<!-- CONTACT -->
## Contact

Project Link: [https://github.com/lisp-stat/sqldf](https://github.com/lisp-stat/sqldf)



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/sqldf.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/sqldf/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/sqldf.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/sqldf/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/sqldf.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/sqldf/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/sqldf.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/sqldf/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/sqldf.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/sqldf/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/

