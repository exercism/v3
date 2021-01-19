## Installing Lua

- [MacOS](#macos)
- [Ubuntu](#ubuntu)
- [Windows 10: WSL](#windows-10%-wsl)
- [Windows 10: Docker](#windows-10%-docker)

#### MacOS

First install Lua and [Luarocks][2] using [Homebrew][1]:

```shell
$ brew install lua luarocks
```

Then install the [Busted][3] testing framework for Lua:

```shell
$ luarocks install busted
```

Then run your tests:

```shell
$ busted
```

#### Ubuntu

First install Lua and [Luarocks][2] using [Apt][6]:

```shell
$ sudo apt-get install lua5.3 liblua5.3-dev luarocks
```

Then install the [Busted][3] testing framework for Lua:

```shell
$ luarocks install busted
```

If this fails, you may need to use `sudo`:

```shell
$ sudo luarocks install busted
```

Then run your tests:

```shell
$ busted
```

#### Windows 10: WSL

First you must enable [WSL (Windows Subsystem for Linux)][7] using [PowerShell][8] (Administrator):

```shell
PS> Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux
```

This requires a reboot.

Next, install Ubuntu from the [Microsoft Store][9], **or** download then install using PowerShell:

```shell
PS> curl.exe -L -o ubuntu1804.appx https://aka.ms/wsl-ubuntu-1804
PS> Add-AppxPackage .\ubuntu1804.appx; rm .\ubuntu1804.appx
PS> ubuntu1804
```

Then update your package list:

```shell
PS> wsl
$ sudo apt-get update
```

Now you are ready to install [LuaRocks][2] and [Busted][3] by following the [Ubuntu instructions](#ubuntu) above.

Once done you can run your tests directly from any Windows command line:

```cmd
C:\> wsl busted
```

#### Windows 10: Docker

##### Install Gitbash

This step is optional. You may have to tweak the following steps slightly if you prefer to use Windows' native CLI or another CLI.

Download [here][10].
Install Instructions [here][11].

##### Install Docker

Follow the instructions to install [Docker for Windows][12].
Once you've finished installing, you can run the following command to ensure it's been installed correctly.

```shell
$ docker --version
```

_You may encounter an error when trying to run Docker the first time._

`cannot enable hyper-v service`

_If this occurs, you can follow these instructions to [enable Hyper V][13]. You may have to disable the setting, restart your computer and re-enable it after boot._

##### Install busted

Once you have Gitbash and Docker installed, you'll be able to install a busted 'container'.

In Gitbash run the following:

Pull down this widely used Docker container by running the following:

```shell
$ docker pull imega/busted
```

Set up an alias, so you don't have to type the whole command every time:

```shell
$ alias dbusted='f(){ docker run --rm -t -v "/$@":/data imega/busted; unset -f f; }; f'
```

Now, in your CLI, you can navigate into the directory you want to run your tests in. Run the following:

```shell
$ dbusted $PWD
```

You should see the output from your busted tests.

[1]: http://brew.sh/

[2]: http://luarocks.org/

[3]: http://olivinelabs.com/busted/

[4]: https://github.com/Olivine-Labs/lua-style-guide

[5]: http://tylerneylon.com/a/learn-lua/

[6]: https://help.ubuntu.com/lts/serverguide/apt.html

[7]: https://docs.microsoft.com/en-us/windows/wsl/faq

[8]: https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-6

[9]: https://www.microsoft.com/en-us/p/ubuntu/9nblggh4msv6

[10]: https://git-scm.com/download/win

[11]: https://www.stanleyulili.com/git/how-to-install-git-bash-on-windows/

[12]: https://docs.docker.com/docker-for-windows/install/

[13]: https://docs.microsoft.com/en-us/virtualization/hyper-v-on-windows/quick-start/enable-hyper-v
