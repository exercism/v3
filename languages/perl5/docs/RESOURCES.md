### Modules

Perl 5 comes equipped with a set of core modules which you can find
listed on [perldoc](https://perldoc.pl/modules). Typically you will be
able to use any of these modules immediately. (Some distributions may
require you to install core modules separately).

[Awesome Perl](https://github.com/hachiojipm/awesome-perl) is a curated list of
useful modules, and also refers to a selection of other lists of recommendations.

There are several module installers available for Perl 5, which will allow
you to install a wide variety of modules available on [CPAN](https://metacpan.org/).

#### App::cpm
A very fast and straightforward module installer.
There is a tutorial available on [CPAN](https://metacpan.org/pod/App::cpm::Tutorial).

You will have this already installed if you followed the `local::lib`
instructions from the [installation documentation](https://exercism.io/tracks/perl5/installation).

Simply run `cpm install` to install required modules from a `cpanfile`,
and use the `--with-recommends` or `--with-suggested` to install recommended
or suggested modules respectively.

Run `cpm install Module::Name` to install a specific module locally, or
run `cpm install -g Module::Name` to install globally.

#### App::cpanminus
The most well-known and commonly used module installer available.
Instructions for installation can be found on [CPAN](https://metacpan.org/pod/App::cpanminus).

Run `cpanm -L local --installdeps .` to install required modules
from a `cpanfile`.
Run `cpanm -L local Module::Name` to install a specific module
locally, or run `cpanm Module::Name` to install globally.

## Code Style and Linting

If you are looking for good general advices regarding Perl coding,
Damian Conway's [Perl Best Practices][PBP] is a good reference.

To prettify your code, you can use the module
[Perl::Tidy][PerlTidy].

You can then use the provided `perltidy` utility program
to reformat your code.

    # modify file in-place and save old version as some_script.pl.bak
    $ perltidy -b some_script.pl

`perltidy` has a *lot* of configuration options to cater to every taste. They
are documented [here][perltidyDocs].

There is also [tidyview], a visual interface that allows you to
tweak the different options and immediately see the result.

If you want to enforce coding practices, there is also
[Perl::Critic][perlcritic]. It comes with the rules described in
the book *Perl Best Practices* mentioned previously, but plenty of
plugins exist, and it can be customized to fit any in-house coding
rules.

To use:

    $ perlcritic some_script.pl

The tool has many, many configuration tweaks, see its
[documentation][perlcriticdocs] for the full scoop on them.


[PBP]:            http://shop.oreilly.com/product/9780596001735.do
[PerlTidy]:       https://metacpan.org/release/Perl-Tidy
[cpanminus]:      http://search.cpan.org/~miyagawa/App-cpanminus-1.7042
[perltidyDocs]:   https://metacpan.org/pod/distribution/Perl-Tidy/docs/stylekey.pod
[tidyview]:       https://sourceforge.net/projects/tidyview/
[perlcritic]:     https://metacpan.org/release/Perl-Critic
[perlcriticdocs]: https://metacpan.org/pod/distribution/Perl-Critic/bin/perlcritic
