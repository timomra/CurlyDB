![Git tutorial for beginners - Part 2: Git repository, working tree and  staging area](https://storage.ghost.io/c/37/42/374202c6-549c-4d6d-829d-4a898a54ae06/content/images/2022/12/1_zpvd5fjZAFGsVAEsvMGKxA.png)

### Pushing to Github

**(preliminary step) create a remote repository on your own device**

> WARNING: avoid making a `.gitignore` or `README.md`, in your local repository, as these can conflict when you try to link to the remote repository



**1. Link it to your local repository**

To link to our CurlyDB, run this command.

` git remote add origin https://github.com/timomra/CurlyDB.git`



To check the linking run:

``$ git remote -v`` 

(this lists the remote repositories your git can see, as well as their URLs)

> origin  https://github.com/timomra/MyGitTutorial.git (fetch)
> origin  https://github.com/timomra/MyGitTutorial.git (push)





**2. Push the commit**

(\*\*Optional step - you will need to do this if your main was called master)

`git branch -M main` 



Now push the current branch to github (***origin*** is the default name for your remote repository, while **main** tells git which branch you want to push)

`git push -u origin main` 

*Will see something like this:*

> Enumerating objects: 5, done.
> Counting objects: 100% (5/5), done.
> Delta compression using up to 12 threads
> Compressing objects: 100% (3/3), done.
> Writing objects: 100% (3/3), 297 bytes | 297.00 KiB/s, done.
> Total 3 (delta 2), reused 0 (delta 0), pack-reused 0 (from 0)
> remote: Resolving deltas: 100% (2/2), completed with 2 local objects.
> To https://github.com/timomra/MyGitTutorial.git
> 0ee1fc9..75eaefd  main -> main
> branch 'main' set up to track 'origin/main'.