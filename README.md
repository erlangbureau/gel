# How to use
Add to configuration file
```
{gel, [
    {repos_dir, "/opt/repos/"}
]},
```

## git init
```
gel:init(Repository)
```
Types:
* Repository = binary() | list() | atom()

## git commit
```
gel:commit(Repository, Commit)
```
Types:
* Repository = binary() | list() | atom()
* Commit = binary() | list() | atom()

## git push
```
gel:push(Repository)
```
Types:
* Repository = binary() | list() | atom()

## git pull
```
gel:pull(Repository)
```
Types:
* Repository = binary() | list() | atom()

## git merge
merge from Branch to 'master'
```
gel:merge(Repository, Branch)
```
* Repository = binary() | list() | atom()
* Branch = 
    binary() == <<"devel">> or <<"master">> | 
    list()   == "devel" or "master" | 
    atom()   == devel or master

## git checkout
```
gel:checkout(Repository, Branch)
```
* Repository = binary() | list() | atom()
* Branch = 
    binary() == <<"devel">> or <<"master">> | 
    list()   == "devel" or "master" | 
    atom()   == 'devel' or 'master'
