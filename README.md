# Mind Traffic Control (in Racket)

A new Mind Traffic Control (command-line todo-queue) in Racket.

### Quickstart ###

Make sure you have Racket Lang and DrRacket installed. [https://racket-lang.org/](https://racket-lang.org/).

    git clone https://github.com/interstar/mtc-racket.git mtc
    cd mtc
    raco exe mtc.rkt
    ./mtc
    

On running, you should see something like this :

    Welcome to Mind Traffic Control
    File is /home/YOURNAME/Documents/todo.txt
    No items

To add an item to the list simply type it and hit return.

MTC normally shows you the *current next item* in your todo-queue and nothing more. It's up to you what you do with it.

The main philosophy of mtc is to focus on doing tasks, NOT on managing todo-lists. That's why it's a minimal command-line interface with some useful defaults for postponing and delaying items. Ideally MTC should be *invisible* 99.99% of the time.

### Configuring ###

MTC defaults to keeping todos in a file called /home/YOURNAME/Documents/todo.txt but you can change this in a configuration file.

MTC's configuration is stored in HOME/bin/.mtc/config.rkt and is represented as a simple Racket dictionary. 

The value **todo-dir** is used to represente MTC's working directory. Eg. a simple config could look like :

    ( (todo-dir . "/home/YOURNAME/mytodos/") )

Save something like this into your config.rkt file, make sure that directory exists, and restart mtc.

### Instructions ###

Type any line over 5 characters long to add it as a new todo item

| command | explanation |
| ------- | ----------- |
| `h`  |  Help. Show this list of commands.
| `s`  |  SAVE ... this is the most important command. If you do not explicitly save, you will lose what you put into MTC. |
| `/`  |  delay the current "next item" ie. push it to the end of the queue. |
| `//` |  push the current "next item" 10 back. Once your queue gets long you have things that ought to be done *"now-ish"*, but aren't your next task. This is for those. |
| `///` | push current "next item" 50 back. When your queue gets big, this becomes meaningful. Depending on how long your tasks are, this is effectively pushing the current one until the end of the afternoon or until next week. If your queue is really big, the single / can push the task months back. |
| `*`  | done / delete. There's no difference. I used to store "done" items. But actually, you really don't care. MTC isn't about admiring long lists of crossed-out "completed" work. It's to manage the things you still have to do. |
| `c`  | count the items in the queue
| `+ TEXT` | finds ALL items that contain TEXT and pulls them to the front of the queue. There is no other "search" or "filter" in MTC. This is how you do search / filtering type stuff. NB .. TEXT can not have any spaces in it. At this time, it's only possible to pull a single search term. However, because pulling preserves ordering, if you pull a term like OFFICE and then pull a term like PARTY, you *will* get the items that match both OFFICE and PARTY at the top of the queue. |
| `- TEXT` | finds ALL items that contain TEXT and pushes them to the end of the queue. The opposite of + ... this is for getting rid of an entire project if you know it's not something you want to think about at the moment. You can always pull it back to the front (ie. prioritize it) later. |
| `l`  | List. You really shouldn't use this command. But occassionally you do need to scan the entire list. l lets you do it |
| `ll` | a quick peek ahead at the first 10 items on queue
| `lll`| peek the first 50 items on the queue
| `\`  | Pull last item to front of queue.
| `\\ ITEM` | Adds ITEM to the front of the queue
| `k* TEXT` | be careful. This is multi-kill or bulk delete. It removes all items from the list that contain TEXT. Useful if you find that you've cancelled a project and want to remove all items from it from the queue. |
| `e EXTRA TEXT` | appends EXTRA TEXT to the next item. There is currently no general edit item in MTC. But it's sometimes useful to be able to append some kind of extra tag or keyword to an existing item.|
| `a`  | Analyze the URL in an item. Currently pulls the URL and shows its title. Useful if you've dropped a link into MTC to read later but can't remember what it is. (EXPERIMENTAL. MAY BREAK!) |
    

### Philosophy ###

An earlier version of MTC was created as a web app. It had some nice features ... postpone until after a particular date, delegate to another user (and track whether that user had done the task). But I became dissatisfied with it in many ways. And the UI is now fairly outdated. But I never lost my enthusiasm for a "queue-based" rather than a "list-based" approach to todos.

At the same time, I admired [todo.txt](http://todotxt.com/) and its minimal command-line philosophy. With this new MTC, I set out to be compatible with todo.txt; not in the sense of providing the same functionality, but in the sense that it can share and operate on the same basic file. If you start using MTC and really don't like it, your todos are still in a plain text file and you can continue working with todo.txt tools and conventions. In fact you should be able to operate on the same file with both todo.sh and mtc in parallel. (Though be careful that MTC stores the list in memory.)

MTC is designed to handle a LOT of todos. Hundreds or potentially thousands. And over long periods of time. The original MTC lasted 7 years. And some items NEVER got done. But they got remembered. I hope that I'll be able to say the same with the new MTC. MTC doesn't want you to see, or think about, or fiddle with, the "list". Just see and do the next item. And do some occassional bulk things that ensure you are getting the right items in front of you.

Note: One difficulty people have when looking at MTC is that they put in 2 or 3 (fake) tasks and think "so what?". MTC doesn't really make much sense or much of an impression with a couple of tasks. If you try it, you keep hitting the `l` key to see the whole list, see the list and then think "Meh! That's less useful than opening the file in a text editor."

So if you *are* trying out MTC, and you have an existing todo.txt or todo-list you can export in plain-text, then my suggestion is to try copying THAT into the todo.txt file, and see how it feels in MTC. The more items you have, the more useful MTC becomes.
  
Finally. MTC has always been also a learning exercise for me. Now it's my exercise for learning Racket and Lispy / Schemish ways of doing things. It will continue to be that. When Lispy comes into conflict with other requirements or nice-to-haves. Lispy and simple win.

### NOTE :: THIS IS A DEPRECATED PROJECT

Note that while MTC in Racket was a very fun and useful project, I have now pretty much abandoned it. 

Not because I've given up on Mind Traffic Control as my todo-queue or as a useful concept. I still use it every day. But because I've now ported it to Clojure. See https://github.com/interstar/MTC-CLJ to get that and see further work in progress.

Obviously if anyone in the Racket community wants to pick this up and continue developing it, I will be delighted. I have a lot of admiration and respect for both the Racket language and community. However, for me personally, Clojure turned out to be a better option for various projects, and so it's more convenient to move MTC into Clojure for consistancy / compatibility, than to continue riding two horses.
