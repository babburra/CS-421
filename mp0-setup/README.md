Objectives
==========

The objective of the MP is to make sure that you can use the build system
(`stack`) and distribution system (`git`) we'll be using throughout the
semester. You don't have to do any programming to complete this MP; you only
have to run a sample program we provide.

Goals
-----

-   Get access to your student Gitlab repository.
-   Get Stack up and running.
-   Run a Haskell program.


GitLab
======

We'll be using GitLab to distribute and collect homeworks from you. You'll need
to make sure that you have some form of Git installed to interact with GitLab.

Git
---

The official [Git
documentation](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
explains how to install Git for each of the major platforms. Git alone is enough
if you are willing to use a CLI. There are many options for GUIs as well; you
can see some of them on the [Git website](https://git-scm.com/download/guis).

For documentation on Git, you can type `man git` (at a suitable command-line),
or look at the [Git documentation](https://git-scm.com/doc). Google and Stack
Overflow are very helpful for debugging Git issues as well (chances are many
people have already had the issue you have).

If you haven't already, you'll need to provide Git with some information about
yourself (see [First-Time Git
Setup](https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup)).
You can run the below commands to do so:

```sh
$ git config --global user.name "YOUR NAME"
$ git config --global user.email "YOUR EMAIL"
```

This is purely for your commit messages, we will not be using this information
at all. If you leave it unset, Git will complain at you.

GitLab
------

UIUC has a GitLab server to manage content for some courses and projects. You
should have received an email inviting you to join GitLab (if you haven't
already signed up). Once your account is setup, you should go to
<https://gitlab-beta.engr.illinois.edu/cs421-su16/NETID> (where you replace
`NETID` with your netID) so see your repository for this class.

You can `clone` your repository via SSH or HTTPS. Instructions are below. Make
sure you have setup your GitLab by logging into the web interface before
proceeding. If you are using a GUI Git client, you'll need to look up how to
clone repositories using that client.

### Cloning (via HTTPS)

You can clone your repository directly via HTTPS (replace `NETID` with your
netID and `short-name` with what you would like to call it):

```sh
$ git clone 'https://gitlab-beta.engr.illinois.edu/cs421-su16/NETID' 'short-name'
```

It will ask for your user-name and password; this is your GitLab user-name and
password (which is usually the same as your school LDAP user-name and password).

### Cloning (via SSH)

If you would like to not enter your password every time you `pull` or `push` to
your repository, you can also setup an SSH key. We assume if you are at this
point, you already know how to generate an SSH key and how SSH works. If not,
there are many guides to setting up SSH key authentication online.

Once you have an SSH key, on the GitLab website you can click the button on the
left panel called `Profile Settings`. Along the top there will be a tab called
`SSH Keys`. You can upload your public key via the given text-area.

To point SSH at GitLab, use the following settings (which can go in your
ssh-config file, usually `~/.ssh/config`):

```sh
host gitlab
    hostname gitlab-beta.engr.illinois.edu
    user git
    identityfile path/to/your/private/key
```

Then, assuming you've setup GitLab properly, you can run (replace `NETID` with
your netID, and `short-name` with what you want it to be called):

```sh
$ git clone 'ssh://gitlab/cs421-su16/NETID' 'short-name'
```


Stack and Haskell
=================

Now that you have cloned the repository, you should be able to see the directory
`assignments/mp0-setup`. In fact, in that directory you should find `README.md`,
which is this document!

We're going to make sure that Stack works on your machine now, which means that
we first need to install Stack.

Installing Stack
----------------

There are several guides online to install Stack on whatever system you have.
[Here is one such
guide.](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)

You need to have Stack installed before proceeding. Make sure you have stack
installed by running `stack --version` at a command line.

Running a Program
-----------------

In the directory `mp0-setup` there is a file `mp0-setup.cabal`. Make sure that
it is there. When we run `stack init` the first time, it will pull in GHC if you
don't have it, along with Stackage packages `base`, `unordered-containers`, and
`parsec`. We'll need these packages throughout the course.

### Initialization

Start by running `stack init`:

```sh
$ stack init

# output (may be different)
Using cabal packages:
- mp0-setup.cabal

Selecting the best among 5 snapshots...

* Selected lts-5.11

Initialising configuration using resolver: lts-5.11
Writing configuration to file: stack.yaml
All done.
```

You will have to run `stack init` for each assignment. It will not pull in
duplicates of dependencies if they already exist on your system.

### Building

Then we proceed with `stack build`, which will build the `main` executable.

```sh
$ stack build

# outputs (may be different)
mp0-setup-0.1.0.0: configure
Configuring mp0-setup-0.1.0.0...
Warning: 'license: NCSA' is not a recognised license. The known licenses are:
GPL, GPL-2, GPL-3, LGPL, LGPL-2.1, LGPL-3, AGPL, AGPL-3, BSD2, BSD3, MIT, ISC,
MPL-2.0, Apache, Apache-2.0, PublicDomain, AllRightsReserved, OtherLicense
mp0-setup-0.1.0.0: build
Preprocessing executable 'main' for mp0-setup-0.1.0.0...
[1 of 1] Compiling Main             ( app/Main.hs,
.stack-work/dist/x86_64-linux/Cabal-1.22.8.0/build/main/main-tmp/Main.o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.22.8.0/build/main/main ...
mp0-setup-0.1.0.0: copy/register
Installing executable(s) in
/path/to/repo/assignments/mp0-setup/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/bin
```

### Executing

Then you should be able to run `stack exec main` to actually run the program. It
will print the message `Welcome!`, then will display the prompt `>`. You can
type various statements at this prompt.

```
$ stack exec main

Welcome!
> print 3
3
> print "hello"
"hello"
> print x
ERROR (environment): No definition for x
> x := "hello"
> print x
"hello"
> y := 5
> print y
5
> quit
Goodbye!
```

This very simple language only supports three types of statements: `print ...`
for printing, `... := ...` for assignment, and `quit` to exit. You can use
natural numbers or strings enclosed in double quotes. Look in the file
`app/Main.hs` if you are interested in how it is implemented. Throughout the
semester, you will be implementing much more sophisticated languages than this,
so it's not bad to take a peek.

### GHCi

You should also try out GHCi; especially for future assignments where you need
to do more debugging. Try using GHCi with `stack ghci` (you can also call `ghci
app/Main.hs` directly):

```sh
$ stack ghci

# outputs (may be different)
Using main module: 1. Package `mp0-setup' component exe:main with main-is file:
/path/to/repo/assignments/mp0-setup/app/Main.hs
Configuring GHCi with the following packages: mp0-setup
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             (
/path/to/repo/assignments/mp0-setup/app/Main.hs,
interpreted )
Ok, modules loaded: Main.
*Main>
```

Here we have loaded our program with GHCi, which means that if you call `main`
it actually launches the program (and you can type the same things as before).
Note that the `*Main>` prompt is *not* from our program, but from GHCi. You can
enter any Haskell query at this prompt and GHCi will compute it for you
(assuming the correct dependencies are loaded). Try out the following examples:

```haskell
*Main> main
Welcome!
> print 3
3
> print x
ERROR (environment): No definition for x
> quit
Goodbye!
*Main> map (+3) [1,2,3,4,5]
[4,5,6,7,8]
*Main> foldr (+) 0 [1,2,3,4,5]
15
*Main> 5 * 8
40
```

One of the most useful features of GHCi is the ability to ask about the *types*
of things. You'll find that it's much easier to program with Haskell if you pay
attention to the types of everything. Try asking for a few types (by using
`:t thing-you-want-type-of`):

```haskell
*Main> :t map
map :: (a -> b) -> [a] -> [b]
*Main> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
*Main> :t (+)
(+) :: Num a => a -> a -> a
*Main>
```

The way to read this is, "the type of `map` is a function which takes a function
of type `(a -> b)` as its first argument, a list of `a` as its second argument
(the `[a]`), and returns a list of `b` as its result (the `[b]`)". The type of
`(+)` is a little simpler, it takes two arguments which must be `Num`, and
returns something of the same type.

If the types don't make sense, don't worry. We'll be learning more Haskell as we
go. We have recommended it before, and we'll recommend it again, but you *really
should read* some of the [Learn you a
Haskell](http://learnyouahaskell.com/chapters) book. It explains things very
clearly. If you don't want to start at the beginning, at least read chapter 3,
the section on [Types and
Typeclasses](http://learnyouahaskell.com/types-and-typeclasses).


How do I get My Points?
=======================

You get your points for this assignment by creating a file `ireadit` in the
`mp0-setup` directory. This file can contain whatever you want, it just has to
exist. It must be in the correct place, and it must be named correctly.

Once you have created it, make sure you `add` it to the repository, then
`commit` and `push` it. An example below:

```sh
$ touch ireadit         # creates empty file 'ireadit' on Linux
$ git add ireadit
$ git commit -m "submitting MP0"
$ git push
```
