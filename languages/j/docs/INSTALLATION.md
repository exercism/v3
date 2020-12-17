Refer to [System/Installation](https://code.jsoftware.com/wiki/System/Installation) for instructions on how to get things set up on your system.

After installing, I'd suggest using the package manager (called `pacman`) to grab available addons. This includes fun bits like plotting, graphics, image manipulation, and much else. To do this, you can start a `J` console session by typing `jconsole` then enter:

```j
load 'pacman'
'update'jpkg''
'install' jpkg '*'
```
