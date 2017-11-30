# waterDE

## Calculation of water retention curves by Differential Evolution

This web site hosts the source code and a few binary packages of `waterDE`, a command line application that calculates the characteristics parameters of water retention models using [Differential Evolution (DE)](http://www1.icsi.berkeley.edu/~storn/code.html).

The  `waterDE` application currently implements the Brooks-Corey, van Genuchten and Rossi-Nimmo analytic models. In the next release, a plugin architecture will allow to easily add a wider range of analytic models to the package.


### Reference
<a id="ref"></a>

The official reference for `waterDE` is:

> S. Maggi, _Estimating water retention characteristic parameters using differential evolution_, Computers and Geotechnics, Volume 86, June 2017, Pages 163-172, doi [10.1016/j.compgeo.2016.12.025](https://www.sciencedirect.com/science/article/pii/S0266352X16303329).

Please cite this paper whenever you use `waterDE` for a scientific publication or a presentation.


### Introduction

Running an application such as `waterDE` requires a basic knowledge of the terminal command line interface of the different operating system, such as the `CMD` or PowerShell command interpreters for Windows or the `bash` shell for Linux and OS X (OS X hab been rebranded macOS in mid-2016, but the most common name for Apple's operating system will still be used throughout this document).

Even if most operations can be performed using the standard graphical user interface (GUI) tools of the operating system, it is still useful to be able to perform basic task from the command line interface, such as copying or moving files, creating directories or executing a text-based computer program.

Excellent tutorials on how to use the terminal command line of the main operating systems can be easily found on the web, such as the [Introduction to the Windows Command Prompt](https://www.bleepingcomputer.com/tutorials/windows-command-prompt-introduction/), [The Windows Command Line](http://commandwindows.com/) and [How PowerShell Differs From the Windows Command Prompt](http://www.howtogeek.com/163127/how-powershell-differs-from-the-windows-command-prompt/) for Windows, or [Using the Terminal](https://www.linode.com/docs/networking/ssh/using-the-terminal), [How to Start Using the Linux Terminal](http://www.howtogeek.com/140679/beginner-geek-how-to-start-using-the-linux-terminal/) or [The Beginner’s Guide to Shell Scripting: The Basics](http://www.howtogeek.com/67469/the-beginners-guide-to-shell-scripting-the-basics/) for Linux and OS X.



### Installation of waterDE

The easiest way to install `waterDE` is to download the precompiled binary packages for Windows, Linux or OS X. Installation instructions for each operating system can be found below.


#### Windows installation

The current version of `waterDE` has been tested for work under Windows XP, Windows 7 and Windows 10, but should run also under Windows 8.x and Vista.

The Windows package is compressed using the `zip` file format and can be extracted by double-clicking on the dowloaded file, selecting Extract All and choosing the destination folder. Windows XP (and above) users can also use the freely available [7-Zip](http://www.7-zip.org/) application.

The package contains the `waterDE.exe` executable program and an `examples` folder containing a few data and configuration files useful to test the application.

No installation is required to use `waterDE.exe`: the simplest way to use it is to copy `waterDE.exe` to the folder where the data and configuration files of a project are stored.

Alternatively, to keep the project folder clean, `waterDE.exe` can be moved to any other folder, provided it is located in the Windows System PATH. This allows to run `waterDE` as it were a built-in program, without worrying about its location in the file system.
Detailed instructions to add a directory to the Windows System PATH [can be found here](http://www.howtogeek.com/118594/how-to-edit-your-system-path-for-easy-command-line-access/).


#### Linux installation

The current version of `waterDE` has been tested for work under the latest stable releases of the Debian, Ubuntu and CentOS distributions.
Since the application is compiled statically, meaning that it self-contained and doesn't require any other files to work, it should also run under any recent Linux distribution.

The Linux package is compressed using the `tar.gz` file format and contains two separate executable programs, `waterDE` and `waterDE_64bit`, for 32-bit and 64-bit systems, respectivey, together with an `examples` folder containing a few data and configuration files useful to test the application.

The easiest way to to check if your Linux system is 32-bit or 64-bit is to launch a terminal emulator (installed by default in all known Linux distributions) and run the following command

	uname -m

that gives either "i686" or "i386" for a 32-bit system or "x86_64" for a 64-bit system.

No installation is required to run `waterDE`: the simplest way to use it is to copy the executable compatible with your computer architecture to the folder where the data and configuration files of a project reside. A 32-bit executable can run on a 64-bit system with a small speed penalty, however the converse is not true and a 64-bit executable does not run at all on a 32-bit system.

Alternatively, to keep the project directory clean, the executable compatible with your computer architecture can be copied to one of the [standard directories](http://unix.stackexchange.com/questions/36871/where-should-a-local-executable-be-placed) used by Linux for user programs not managed by the package manager of the distribution, either `/usr/local/bin` or `~/bin/`, where the `~` symbol indicates the home directory of the current user.
This allows to run `waterDE` as it were a built-in program, without worrying about its location in the file system.

The whole extraction and configuration process can be performed using the GUI tools available in your distribution or by running the following commands from a terminal emulator

    # create the ~/bin directory if it does not already exist
    if [ ! -d "~/bin" ]; then cd ; mkdir bin; fi
    # change the path if the package has been downloaded elsewhere
    cd ~/Downloads
    # extract the compressed package file
    tar -xzf waterDE-linux.tar.gz
    # copy the two executable programs to the ~/bin directory
	cp -p waterDE waterDE_64bit ~/bin

If you are sure about the architecture of your system, you can copy only the right executable to `~/bin` directory.



#### OS X installation

The current version of `waterDE` has been compiled to run under the latest versions of OS X (or macOS), from 10.8/Mountain Lion to 10.12/Sierra. To check the version of Mac OS X, click on the Apple menu () on the left of the menu bar and choose About This Mac. The version number appears beneath "macOS" or "OS X."

The OS X package is compressed using the `tar.gz` file format and contains the `waterDE` application in the standard OS X format, a bash script `waterDE.sh`, to ease the use of the application from the command line and an `examples` folder containing a few data and configuration files useful to test the application.

For reasons related to how OS X handles application packages, the OS X package requires a well-defined installation procedure, that can be handled in part from the Finder (the standard graphical tool used by OS X to manage files) and in part from the `Terminal` application, the default terminal emulator for OS X.

For the first stage of the installation

- download the compressed `waterDE` package for OS X, `waterDE-osx.tar.gz`, into the Downloads folder of the current user's home directory;
- click on the Finder icon and select the Downloads folder;
- double-click on the `waterDE-osx.tar.gz` file and wait until the Finder expands it;
- double-click on the new `waterDE-osx` folder and drag the `waterDE` application (the file with the blue icon) to the Applications folder, the default folder where OS X applications are located.

Now launch the `Terminal`, located in in Applications > Utilities, and run the following commands,

	# be sure to be in the current user's home directory
	cd ~
    # create the ~/bin directory if it does not already exist
	# and hide it from the Finder
    if [ ! -d "~/bin" ]; then cd ; mkdir bin; chflags hidden ~/bin; fi
    # copy the bash script that starts waterDE to the ~/bin directory
	cp -p ~/Downloads/waterDE-osx/waterDE.sh ~/bin
	# add the ~/bin directory to the $PATH environment variable
	echo "export PATH=~/bin:$PATH" >> .profile

To immediately load the updated bash settings file without having to end the Terminal session, run

	source ~/.profile

otherwise, simple close and reopen the Terminal again.


### Compilation of waterDE

The `waterDE` program is written in Fortran 90 and can be compiled using any modern Fortran compiler, such as the [`gfortran` ](https://gcc.gnu.org/fortran/) compiler of the GNU Compiler Collection (GCC) or the excellent [Intel Fortran ](https://software.intel.com/en-us/fortran-compilers) compiler.

Compilation of `waterDE` can be done easily on Linux, OS X and Windows with the provided Makefile.
Users of other operating systems should uncomment the lines marked by `###`, adding the proper compiler and compilation switches for their platform.

Windows users are recommended to compile `waterDE` using either one of the well-known [MinGW](http://www.mingw.org/) or [Cygwin](https://www.cygwin.com/) tool sets, that provide a Linux-like development environment for Windows XP and above.
After compilation, the `waterDE.exe` executable does not need the development environment and can be run on any Windows machine with a compatible architecture.
It should also be possible to compile waterDE using the recent [Bash shell](http://www.howtogeek.com/265900/everything-you-can-do-with-windows-10s-new-bash-shell/) for Windows 10, however the use of this tool is left to experienced users.


### Usage

The `waterDE` application is designed to be executed from a terminal command line and requires only a data and a configuration file, located in the same _project directory_. The name of the configuration file must be equal to that of the corresponding data file, with extension `.conf`.

In the following, it is assumed that the data and configuration files are saved under the names `example.dat` and `example.conf`, respectively.

<!-- From now on, the generic names `example.dat` and `example.conf` will be used for the data and configuration files, while the directory that contains the data and configuration files will be the _project directory_. -->

The data file is composed of pairs of measured `psi` and `theta` values (see the [reference](#ref) for the meaning of the symbols) separated by an arbitrary number of spaces or tabs. The data file can also contain an arbitrary number of header lines describing the measurement conditions. All header lines must start with an hash `#` character.

The configuration file is divided into two separate sections:

- The `general` section sets the random seed used to start the calculations, the analytic model to use for the calculations and lower and upper bounds of each parameter. The settings for the number of parameters and for the optimization method must be kept at their default values. For convenience, the three implemented analytic models and a set of default upper bounds for each model are already inserted in the given example configuration files, where a `!` symbol is used to skip the lines of the configuration files that should not be read by `waterDE`.

- The `de` section sets the parameters for the DE algorithm. Most of the parameters there can be left to their default values. The only parameters worth changing are the size of the `population` (usually 10 times the number of parameters but it can be useful to explore different values) and the number of `generations` over which to perform the calculations. The `refresh` parameter sets the number of generations after which the current values of the model parameters calculated by DE are printed on the terminal and in the log file (see below). If one is interested only to the final results, the  `refresh` parameter should be equal to the number of `generations`. Otherwise, if one wants to inspect how the calculations converge to the optimal values, it is suggested to set `refresh` to at least 1/10 or 1/20 of the number of `generations`.

When working with several different data files, it is also possible to use two separate configuration files: a standard configuration file containing the parameters common to all the data files, and a second configuration file (with tha same name of the data file) containing only the parameters specific to the corresponding data file. An example of the use of two configuration files is given below.

The `examples` directory provided with ethe `waterDE` package contains a few data and configuration files useful for testing the progra and to compare the results with those reported in the [reference paper](#ref) above. It is worth noting that the last digits of the calculated model parameters might differ slighty from the reported values because of the different random seed used for the calculations.


#### Basic command line usage
<a id="basic-usage"></a>

The basic command line entry of `waterDE` is (in the following, Windows users should replace `waterDE` with `waterDE.exe`)

	waterDE example.dat

which starts the application (provided it is in the system path), reads the data file `example.dat` and the corresponding configuration file `example.conf` and writes a few output and log files:

- `example-data.out`, contains the original _psi_-_theta_ measured values;
- `example-DE.out`, contains the corresponding _psi_-_theta_ values calculated from the model parameters determined by waterDE;
- `example-DE_params.out`, contains the set of calculated parameters of the analytic model, where _b1_ and _b2_ correspond to _theta\_r_ and _theta\_s_, respectively, _b3_ can be either _alpha_ or _psi\_e_ and _b4_ can be either _n_ or _lambda_, according to the selected model;
- `example.log`, contains a complete record of a waterDE run and comprises all the output files generated by the program.

To avoid overwriting the results of a previous run, a calculation can be repeated with the same data file only after having renamed, moved elsewhere or deleted, the previous set of `.out` and `.log` files.


#### Advanced command line usage

The complete command line entry of `waterDE` is shown below, where the arguments in square brackets are optional. This usage information can be shown by running `waterDE` with the `-h` (or `--help`) option.

	waterDE --help
	 usage: waterDE [-h] [-c CONFIG] [-l LOG] [-e] [-f] DATA

	 mandatory argument:
	   DATA                  input data filename

	 optional arguments:
	   -h, --help            show this help message and exit
	   -c CONFIG, --config CONFIG
	                         use common CONFIG configuration file
	   -e, --extdplot        save extended output data file
	   -f, --fitness         save fitness history
	   -l LOG, --log LOG     save all output to LOG file


The only mandatory argument is the filename of the input DATA file (e.g., `example.dat`, with the filename convention used here).

When `waterDE` is run against the given data file, it looks for a configuration file with the same name of the data file, and extension `.conf`. If the configuration file is not present in the same directory of the data file, it writes an error message and stops.

The `-c` (or `--config`) command switch allows to define the filename of a configuration file containing a set of parameters common to several data files. The parameters that are specific to each data file are defined in the configuration file named after the data file and having extension `.conf`.

The `-l` (or `--log`) command switch allows to specify the filename of the LOG file.

The `-e` (or `--extdplot`) command switch writes an output file containing an extended set of _psi_-_theta_ values, calculated from the model parameters determined by waterDE.

The `-f` (or ` --fitness`) command switch writes an output file containing the full record of the fitness calculated by waterDE at each generation.


#### Pratical examples

All the usage examples use the data and configuration files contained in the `examples` folder.

To run these examples, copy the `examples` folder anywhere in your filesystem and launch the preferred terminal application for your system.

**Basic usage.** Run `waterDE` from the project directory, specifying only the name of the data file

	waterDE 1330.dat

The results of the calculations are printed on the terminal screen and also on the files `1330-data.out`, `1330-DE.out`, `1330-DE_params.out`, `1330.log`. The content of each of these files has been described in the [Basic command line usage](#basic-usage) section.

**Custom log.** The `-l` (or `--log`) command line switch
allows to specify the name of the log file instead of using the standard filename derived from the name of the data file

	waterDE -l 1330-run001.log 1330.dat

Since the log file contains the full record of each `waterDE` run, this option can be useful to repeat the calculations on the same data file with different sets of configuration parameters.

**Extended output.** The `-e` (or `--extdplot`) command line switch

	waterDE -e 1330.dat

writes, in addition to the standard files specified above, a file `1330-DE_extdplot.out` containing an extended set of psi-theta values, calculated from the model parameters determined by waterDE. This file can be useful to plot the calculated model curve against the original data set.

**Fitness history.** The `-f` (or `--fitness`) command line switch

	waterDE -f 1330.dat

writes, in addition to the standard files specified above, a file `1330-DE_fitness.out` containing a full record of the values of fitness calculated by waterDE at each generation.

**Double configuration.** When working with several data files it might be useful to define two different configuration files: a file containing the parameters that are common to all the data files and another file named after the corresponding data file that contains only the configuration parameters specific to each data file.

The `examples` directory contains an illustration of the such a setup, where the `common.conf` contains the parameters that are common to all the data sets, and the `13300m.conf` and `4440m.conf` files contain the parameters that are specific to each corresponding data file.

The double configuration setup can be run with the commands

	waterDE -c common.conf 1330m.dat
	waterDE -c common.conf 4440m.dat

where the `1330m.dat` and `4440m.dat` files are copies of the `1330.dat` and `4440.dat`, and have been renamed to avoid conflicts with the original single configuration files.


### License

Copyright ©2016-2017 Sabino Maggi.

This work is released under the terms of the [GNU General Public License version 3.0+](https://www.gnu.org/licenses/gpl.html). Copyright and license notices must be preserved throughout all derivative works.
