# How to start with CUPID
Written by [Hieu Le](https://github.com/hieulel)

This document is made for beginers who do not experience in:
* UNIX environemnt
* Version control, Git
* The structure of the CUPID documentation

Here I show you how to initate your knowledge on UNIX environment, version control and the structure of the source code.

## UNIX Environment
Unix environment is an operating system consisting of three important features; a kernel, the shell and a file system.

1. Window OS
Since Unix is not native on Window OS, I have installed on your computer equipped with Ubuntu and and UNIX command line (including `gfortran`)
2. Mac OS
It is built-in and I installed `gfortran` on Terminal already.

### Only remember these:
On your terminal (or Command Prompt, i.e. `cmd` on Window OS)

#### Go to a directory `cd` and list `ls`
```shell
cd CUPID #go to CUPID folder 
cd CUPID/src #go to CUPID folder, then folder src
cd .. #go back
ls #list the directory
ls -li #list the information of the directory with permission control
```
Visit this video for UNIX environment permission
[Bash Permission control](https://youtu.be/oxuRxtrO2Ag?t=3400)

<iframe width="560" height="315" src="https://www.youtube.com/embed/oxuRxtrO2Ag?start=3400" frameborder="0" allow="accelerometer; autoplay=0&rel=0; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
#### Create `mkdir` `touch`, remove `rm` and move `mv` file or directory
Remmember, you can use these command for **multiple file and folders**
```shell
mkdir foldername #create folder with name
touch filename #create file with name
rm filename #delete file
rmdir foldername #remove EMPTY directory, 
rm -r foldername #remove directories with sub-directories with confirmation
rm -rf foldername #remove directories with sub-directories with no confirmation prompt
mv filename foldername #move file to a folder, this can be apply for folder as well.
```

## Version control, Git
Git is  

### How to start?

### The golden triangle

#### Add

#### Commit

#### Push

* * *

## Structure of the CUPID Documentation

### Subroutine

### Code structure 
