
# Table of Contents

1.  [Welcome New Org Mode Users](#orgbc946a5)
2.  [What Is This And Why Do I Need It?](#org750ce80)
3.  [About This Repo](#orgcec9542)
    1.  [Overview](#org193ee78)
    2.  [Installation](#org49c1d1e)
4.  [I've Installed It, Now What?](#org2f35e71)
    1.  [An Intro to this Org Mode Tutorial](#orge23fc91)
    2.  [Tracking Work with TODOs](#orgefd1304)
    3.  [Understanding the Agenda](#org7bab212)
    4.  [Using Capture Templates](#orge4d3d98)
5.  [A Practical Walkthrough](#orgcb2876a)
    1.  [Monday Morning Meeting](#org6c5418b)
    2.  [Filling in the Action Items](#orge51b240)
    3.  [View the Agenda](#orgeaf9b69)
    4.  [Start Working on TODO #1](#org28b9b5e)
    5.  [Start Working on TODO #2](#orgdcf489e)
    6.  [Start Working on TODO #3](#orgad203fc)
    7.  [Start with a simple TODO](#orgf3e7d4b)
    8.  [Start working on the TODO](#orgc63ad87)
    9.  [Make a note](#orgdf5a0c5)
    10. [Record a journal entry](#orgd261735)
    11. [Filter the agenda](#orgfddfb04)
    12. [Recap](#org1958fe8)

****Quick Start Guide****
For more details see below, but this is the minimum you need to make this work.

1.  Backup your `.emacs` file if you have one (`mv ~/.emacs ~/.emacs.bak`)
2.  Download the `.emacs` file from this repo
3.  Move it to your home directory (`mv ~/Downloads/.emacs ~/`)
4.  You are done, now restart `emacs`


<a id="orgbc946a5"></a>

# Welcome New Org Mode Users

So you want to use Org Mode, but you don't have 2 years to devote to teaching yourself Lisp? No problem. This repo was created just for you. Here you will find a simple solution that requires no additional lisp configuration on your part to work. It is ideal for users who are completely new to both Emacs and Org Mode. Simply drop this config file into your home directory, restart Emacs, and you are ready to go.

If you came here looking for a more in depth explanation of some of Org Mode's core features, please check out the very detailed write up I did in my [Emacs Org Mode Tutorial](https://github.com/james-stoup/emacs-org-mode-tutorial/). For the new people, you don't need to read that document (it is huge and can be daunting) as it is designed for people who want to really customize their Org Mode configurations. However, it does provide a lot of useful explanations with screenshots, so maybe come back to it once you've gotten some more experience with Org Mode.


<a id="org750ce80"></a>

# What Is This And Why Do I Need It?

A lot of Emacs related documentation assumes you already know quite a bit about the system and the documentation that is geared for new users is often so densely packed with information that it can be difficult to orient yourself. In short, it can be overwhelming. This is especially true for new users who don't care about "THE POWER OF EMACS" but instead just want to try out Org Mode.

The good news is that Org Mode "just works" right out of the box. The bad news is that the defaults are not great and without a lot of customization, it isn't clear **at all** that any of this stuff is actually useful. I discovered this first hand when I helped a new user get started in Org Mode. My `.emacs` file is over a thousand lines long with the Org Mode section easily spanning several hundreds lines. So when this new user saw my version of Org Mode and compared it to their version of Org Mode, they were rather disappointed. Where was the magic? Why did mine look fancy and theirs look so plain? And what did they have to do to get from where they were to where I was. I could have told them to just play with it for a year or two and then they would eventually get an idea of what they needed, but I knew if I did that they would just find another solution. What to do?

I sent them the link I put above so they could check out my tutorial on designing your Org Mode configuration from scratch, but they didn't want that. In fact, they had no idea what they wanted. The only thing they were sure of was that they wanted it to be functional now. Not in a year, not after they had learned lisp, right now. So what I did was massively pare down my own Emacs configuration until it was a lean setup geared entirely towards Org Mode. No code completion, language servers, linters, git integration, or anything else that wasn't directly related to Org Mode. Then I further cleaned it up to make it as easy to use as possible. Finally, I helped them install this new config and gave them some minimal instructions on how to use it. Guess what? They loved it. Based on their response I realized that there were probably a lot of people who would appreciate a little help getting started with Org Mode. So if that is you, I think this repo might help you.


<a id="orgcec9542"></a>

# About This Repo

This repo is designed to help brand new Emacs users become productive in Org Mode. This is not an Emacs tutorial, nor is it meant as an all encompassing Org Mode tutorial. I will explain all the basic key commands you will need to get started, but this is not a substitute for reading the [Official Org Mode Manual](https://orgmode.org/manual/).


<a id="org193ee78"></a>

## Overview

The `.emcas` file has two core parts to it. The first part deals with improving how Org Mode looks and functions at a basic level. The second core part of this configuration is the default values provided. For more info and specific implementation details, look over the `org-better-defaults.org` file in this repo.

Here is a list of most of the primary enhancements provided:

**Improvements**

-   sets up default package repos
-   performance enhancements
-   auto completion
-   minor usability tweaks
-   treemacs (helps visualize headings in org files)
-   defines default org mode directory to be `~/org`
-   improved Keybindings
-   default indentation
-   auto lists
-   auto timestamps
-   expanded TODO options
-   4 custom capture templates
-   7 custom tags
-   1 custom agenda
-   colorized TODOs
-   colorized Tags
-   better fonts
-   better bullets


<a id="org49c1d1e"></a>

## Installation

To make your version of Emacs and Org Mode look like the screenshots below, download the `.emacs` file in this repo and either copy it into your existing `.emacs` file or overwrite it entirely. Then restart Emacs, open (or create) an Org file and you should a much nicer looking version of Org. To verify Org looks different you can download the `org-better-defaults.org` file in this repo and open it.


<a id="org2f35e71"></a>

# I've Installed It, Now What?

If you've already installed the config file and restarted Emacs, then it is time to begin your journey into productivity! As a side note, there are many more features of Org Mode that are not covered here (such as Org Babel, Org Roam, etc.) that greatly expand what you can do. Understanding them is not required for this tutorial, however exploring them on your own is encouraged. Regardless, you should absolutely check out the Org Manual for detailed instructions and expanded examples.


<a id="orge23fc91"></a>

## An Intro to this Org Mode Tutorial

In this tutorial I am going to use the recurring example of someone who wants to use Org Mode to manage tasks at work. I have found that without a realistic example, many tutorials just become too abstract and confusing. So for the duration of this section each of these three concepts will be explained in relation to how they could support a workflow at a generic job. By the end of this Tutorial you should have enough knowledge to create your own TODOs, track them in the Agenda, create new entries with the custom capture templates, and make use of tags to organize everything.

To start off we are going to talk briefly about the three core concepts that make Org Mode work.

-   TODO Items
-   The Agenda
-   Capture templates

Each of these topics will be covered below in more detail and by the end of this document, you should have a good idea of how to use the basics of Org Mode.


<a id="orgefd1304"></a>

## Tracking Work with TODOs

The most basic component of Org Mode is the TODO, it describes something you want to accomplish. It can have lots of details such as when it was created, the current status, the importance, details, checklists, or the tags associated with it. Using TODO items you can keep track of tasks for work, chores to accomplish, or steps in a project, to name a few. A TODO item goes through a series of states to denote the progress you've made. By default the only two states are TODO and DONE however, this config file adds several new states which allows for expanded use cases.

Here is an example TODO item entry that might appear in your `todos.org` file.

    * TODO Create 3rd Quarter Presentation :finance:gary:
    I need to create a presentation detailing the end of quarter numbers. The regional manager Gary will be there so it should be impressive. This presentation also needs to include projections for the next two quarters.

Here is what this looks like in the default Org Mode view:

<pics of presentation>

Here is what it looks like with the Org Mode Better Defaults:

<better pic>


<a id="org7bab212"></a>

## Understanding the Agenda

The agenda is a way of viewing all of your TODOs in one easy to read view. The agenda collects all of your various TODOs and then organizes them so you can easily see what you've done, what is coming up, and the states that everything is in. So if you have 3 TODOs in a file called `work.org` and 5 TODOs in a file called `personal.org`, the agenda view will show you all 8 of your TODOs in one place. It also provides a handy calendar so you can view TODOs that have deadlines or see when you completed a TODO.

Here is an example of the agenda in action.

<pic of the agenda view>

To bring up the Agenda hit `C-c a` and then select the agenda view you want to use. To follow along with this tutorial, use the `Daily Agenda and All TODOs` option by typing `d` at the agenda prompt. Don't worry if this doesn't seem to make sense right now. This section only contains brief explanations of what the core concepts, not a detailed breakdown of every feature. In the Practical Walkthrough section below all of the steps needed to interact with Org Mode will be clearly labeled so that even a total novice can use it.


<a id="orge4d3d98"></a>

## Using Capture Templates

So to quickly recap, TODOs contain your data and the Agenda is how you view your data, but how do you enter it? Well, there are two ways. The first way is to open the file, navigate to the end of it, hit return, type a `*` hit space, type `TODO`, and then enter all of your information. This will absolutely work and if you feel more comfortable doing it this way at first, go for it. The Agenda view will still pick up your manually entered TODOs and display them just fine.

However, there is a better way. Let's say you decided to collect all of your sticky notes, random emails, and hand written notes so that you could create a bunch of TODOs. It would get tedious fairly quickly having to repeatedly enter the same basic format of a TODO over and over when all that really changed between TODOs is the data you are entering. It sure would be nice if there was a way to make that easier&#x2026;

Welcome to Capture Templates. This is a built-in way of bringing up a list of templates that allow you to rapidly enter the type of data you want without having to manually enter the boilerplate associated with it. By default, Org Mode only has template, a generic task. But we can do better than that. In this config there are 4 capture templates to choose from:

-   TODO (t)
-   Journal Entry (j)
-   Meeting (m)
-   Note (n)

Each one captures a specific type of data into a different org file. To access a capture template first press `C-c c` to bring up the list of available templates. Then press the letter designating the template you want to launch. The shortcut letters are shown beside their template in the list above.


<a id="orgcb2876a"></a>

# A Practical Walkthrough

At this point you should have installed the new config file and read over the summary describing the core features of Org Mode. Now we are going to walk through exactly what you have to do to put it all together to get the most from Org Mode. In this Walkthrough we are going to use Org Mode to help us manage the workload of an example job.

In this example scenario it is Monday morning and you are about to have a meeting with several people to discuss possibly making a new purchase order for a large number of components that will be needed for the next quarter. Based on the results of that meeting you might have to speak to some other people, gather some data, write a report, and then submit your findings to your boss by Thursday so she can review it and send the order in by Friday. We are going to walk through each step of this example work week and show how to organize your work and track your progress using Org Mode.

To start this tutorial just launch Emacs. You don't have to open a specific file because the Org Mode Capture Template can be invoked from anywhere. Once you launch Emacs you will see two panes in the window. On the left you will Treemacs showing you a folder named `org` and on the right you will see the `*scratch*` window with the message "Welcome to Emacs!" Click in that pane. This is where you will be doing most of your interaction for this tutorial.

<FRESH EMACS INSTALL PIC>


<a id="org6c5418b"></a>

## Monday Morning Meeting

It is Monday morning and you are about to join a meeting with your boss along with a few coworkers to discuss a potential future purchase order. Your company needs to make sure that certain parts are ordered in time. However, these parts are both expensive and difficult to make. If you don't have enough parts by the time the assembly team needs them then all work will have to stop for a month while more are made. However, if you order too many, it will be very costly to store them until such a time as you will use them. So a plan needs to be decided on in this meeting so you know what to do. Before the real meeting starts you decide to track things in Org Mode. To do that you will need to create a new meeting.

Start by pressing the `Control` key and then while you are still pressing it, press the `c` key. Release both keys. Once you do that, press the `c` key again. This will launch the Capture Template. To make reading this easier, future key combinations will be represented in the standard Emacs form. In this case, the key combination would be written as `C-c c` which is read as "press Control and c, then release both keys, then press c".

<CAPTURE TEMPLATE PIC>

Once you have launched the Capture Template you will be given a choice of four templates to choose from. Since we want to create a new meeting item, press the `m` to select a new Meeting. Once you do this you will notice a strange looking Meeting template appear and the mini buffer will become active with the label `Tags:`. For this example just type in `planning` and hit return.

<INTERMEDIATE CAPTURE TEMPLATE PIC>

At this point you will see an empty (but properly formatted) meeting template with the cursor located at the top heading. Enter a title for your meeting. Since this is a planning meeting let's call it `Purchase planning`. After that you see a dash under the heading `Attendees`. Put your cursor on that line and enter the name of the person leading the meeting, which would be your boss, Alice. After you type `Alice` press return and you will notice another dash has appeared. Go ahead and type `Bob` and hit return. Round out the list with your final coworker `Carla` and then hit return twice to stop making a list.

Now the meeting has started so navigate to the `Notes` section and begin filling in details. Here you might put something like `Alice needs to have a report by Thursday at 11am at the latest. Talk to Bob first, he can find out how many parts we have. Carla knows how many parts we will need for next quarter's projects. I can find out how much of our budget is left for future expenditures by looking at our accounts.` Of course you can take whatever notes you want in whatever structure you want, but for the moment we are keeping it simple since this is a quick meeting.

At the end of the meeting Alice tasks you with getting the numbers from Bob and Carla, checking our available funds, and writing up a recommendation to her by 11am Thursday morning. Now that we know what our tasking is, we need to decide how we should store this info. Thankfully our meeting template has one more heading, `Action Items`! Action Items are tasks that get assigned to us in meetings. Not every meeting will produce Action Items, so for those that don't we can just delete those lines before we save our meeting.


<a id="orge51b240"></a>

## Filling in the Action Items

For our first action item we are going to create a TODO for finding out the current stock levels. Fill out the rest of the first action item with this `Get current stock levels from Bob` and then hit `return`. On this new line let's add some details such as `I need to talk to Bob and get a detailed breakdown of how many parts we have used this quarter as well as how many are left.`. We now have our first action item filled out.

For the next TODO type `C-return` to start a new heading and then type in `TODO [#A]` to create the TODO. Call this action item `Get projected orders from Carla` and then hit return. The details of this will be `Talk to Carla to get the next quarter's projections. I need to know how any parts we are projected to use as well as how many we can lose and still meet our critical orders.` and upon typing that, hit `C-return` one last time to create the final TODO.

Once again type in the TODO template that you used for the previous item. The title of this TODO will be `Compile results and send final report to Alice`. Hit return to get to the details of this TODO and add the lines `Collect all relevant data, summarize it, add my recommendations, and then email it to Alice before 11am on Thursday.` By the end you should have something that looks like this:

<PIC OF MEETING BEFORE FILING>

Now that all the action items have been finished and the meeting is over, we can save this meeting. To do that hit `C-c C-c` and it will file it away.


<a id="orgeaf9b69"></a>

## View the Agenda

At this point we have create a meeting and 3 TODOs, which means our Agenda View will now have something to display. Let's open up the Agenda View and see what it looks like. To launch the Agenda hit `C-c a` to bring up the Agenda selector and then press `d` to activate the Daily Agenda view. Your screen should now look like this:

<DAILY AGENDA AND ALL TODOS PIC>

As you can see, the new TODO items we just created are there. Click into the Agenda view and move your cursor onto the line with your first TODO like so:

<PIC OF CURSOR ON AGENDA>

Once it is there, hit the `TAB` key and it should jump you to the source of the TODO. Any item in the Agenda view can always be followed back to its source. Remember, the Agenda View isn't really for editing things, it just compiles a list of things that already exist.

You might be wondering where the meeting you just created is. After all, you see the 3 action items, but where is the meeting that you were just in? Well, the meeting won't show up by default for reasons that you don't care about right now. However, there is a really useful setting we can turn on to show extra details. Navigate to the `Week-agenda` and put your cursor anywhere within the calendar. This won't work if your cursor is in the TODO sections as this is strictly a calendar setting. Once there, hit `v` to bring up the View options in the mini buffer. We want to enable the log view, so to do that, hit the `l` key. Now we see our meeting~ And as a bonus we also see the time we spent in this meeting. When you first started the capture template for the meeting a clock was started. When the meeting was saved (or filed) the clock stopped and the elapsed time was computed. The agenda view can then take this info and display it in a nicely formatted view. This is especially helpful when you have a week full of meetings. You can see at a glance how much time you spent in meetings and what those meetings involved. Hitting the `tab` key when on the same line as a meeting will take you to that entry.


<a id="org28b9b5e"></a>

## Start Working on TODO #1

You should now have 3 different TODOs to visible in your Agenda. Let's start with the first one, requesting info from Bob. If it isn't open already, go ahead and open up the `meetings.org` file and navigate to the first TODO. Right now it is still in the `TODO` state, but since we are now working it, we need to change it's state. While your cursor is on that TODO heading (it can be anywhere on the line) hit `C-c C-t` to change the state. The mini buffer will pop up and give you a bunch of options you can change it to. In our case we want to set it to `IN-PROGRESS` so hit the `i` key. When that happens a new buffer will appear asking you to insert a note for the state change. At the prompt enter the text `Reaching out to Bob first` and hit `C-c C-c` to save the note.

Once you do this you should be returned to your original buffer where you can see that the TODO named `Get current stock levels from Bob` looks a lot different now. The state has changed from `TODO` to `IN-PROGRESS`, there is a timestamp denoting the state change, and now your note appeared too. Let's add some more details by navigating to the end of this TODO and hit return twice. On this new line we can add more notes. So enter our latest note `Emailed Bob and requested more info.` which describes the work we are doing. For the sake of this example we are going to assume you then opened up your company's email client and sent Bob an email asking for the specific data you needed for your report.

So far so good. We are making progress! Alas for us we get an auto generated email from Bob alerting us to the fact that Bob is out for the rest of the day due to a medical procedure. Since he won't return until tomorrow, we can't do anything else on this front. That shouldn't be a big problem though, since it is only Monday, he will be back on Tuesday, and the report is due Thursday, we still have time. Before we move on to another TODO item, we should really mark this to indicate what happened. To do that put your cursor back on the `IN-PROGRESS` line and hit `C-c C-t` to bring up the state change buffer. Now we are going to hit `b` to set it to blocked. Once again a we are going to add a note to describe this state change. So add the text `Bob is out of the office until tomorrow` and then hit `C-c C-c` to save it. Now our task is set to `BLOCKED` and we have a nice note describing why. Since we can't do anything more here, let's move on to our next TODO item.


<a id="orgdcf489e"></a>

## Start Working on TODO #2

We hit a dead end on our first TODO so now we are moving on to the second task. Just like in the previous TODO we want to change the state from `TODO` to `IN-PROGRESS` and add a note. Reread the previous section if you forgot how to do that. For the note you can use `Talking to Carla since Bob is gone` and then save it like you did the last one. So at this point we have 3 TODOs that are all in different states. Click over to your Agenda pane (if you closed it you can easily reopen it with `C-c a d`) and then type `r` to refresh the view. You should now see that your 3 TODOs are indeed all in different states.

As we did before, navigate to the end of the body of this TODO, it return twice, and update it with what you are currently doing. Here we could add something like `Emailing Carla for her data while I wait for Bob to get back to me.` which summarizes the steps you are taking. Once again we assume that you contacted Carla over your company's email system and requested the information you require. However this time we are in luck because Carla responds fairly quickly with a link to the company's intranet where the data you want is stored. You click on the link, view the data, extract what you need, and add it to the Word document you working on. Of course, you could write everything in Org Mode and then export it to one of a dozen formats (and in fact this is what I do for my job) but for beginners I would use Org to track my notes and then something like Word to format the actual document.

It is important to note that Carla sent us a very important piece of information (the URL to the document) that is only contained in her email. While this is fine for today's task, this might be a document we want to reference later. We could save it to our local file system, bookmark it in our browser, or flag the email, but all of those solutions lack context. It is incumbent on you to remember the details of why you have this file saved. We could just copy and past the URL into our notes, but there is a better solution.

The URL in question is `http:intranet.company.com/financial/reports/Q3-reports.xls` and we want to save this to our current TODO. Start by copying the URL and then navigating to the end of the TODO. Hit return twice to start a new line and then type `C-c C-l` to create a new link. The mini buffer will appear with link options, but you are going to type `C-y` to paste your copied URL in. Once it is pasted, hit return. Now the mini buffer will prompt you to enter a description. Here you can type `Intranet link to Carla's planning document` and hit return. This is now a functioning URL that can be clicked on to open the link in your default web browser.

Since Carla has responded with the information we needed and we logged the results of that conversation, this TODO is complete. All we should have to do is mark it as `DONE` and we can move to something else. To mark it as done go to the `IN-PROGRESS` line and hit `C-c C-t` to open up the state selector. From here type `d` to select done. Enter a note such as `Got everything I needed from Carla` and then hit `C-c C-c` to save it. This TODO is now complete and will appear so in the Agenda View once it is refreshed. 


<a id="orgad203fc"></a>

## Start Working on TODO #3

Before we do any work on this task we need to update the meta data associated with it. If you recall from the description this is due on Thursday (remember in this example it is Monday) at 11am. And while it is good that we recorded this deadline in the body of this TODO, it would be even better if there was a way of integrating this information into our TODO so it could be tracked in the Agenda View. Well good news for us, there is a way of handling deadlines.

Start by navigating to the third and final action item called `Compile results and send final report to Alice` and hit `C-c C-d` to bring up the deadline selector. Now there are a lot of ways we could enter a date. We could type it out in one of several formats or we could click on the desired day in the calendar up top. However, we are lazy so we are going to specify the deadline in the easy manner possible. Since it is Monday and we want to make our deadline for Thursday at 11am, when the mini buffer appears so we can select a date, simply enter `+3d 11am` and hit return. This creates a deadline 3 days in the future at 11am. Now when we refresh the Agenda View we will see that this TODO is special! Because it has a deadline marked on it.


<a id="orgf3e7d4b"></a>

## Start with a simple TODO

You start the day and realize that you need to write a report, send it to your coworker for review, and then submit it to your boss by the end of the day. There is an email detailing notes about this report and you were in a meeting yesterday in which you took some notes too, so you want to consolidate everything in one place. To accomplish this we are first going to create a new TODO item.

<EMPTY TODO PIC>

You will now see a new TODO template and your cursor will be on the heading line. Now we can enter a heading. Type in `Write report on future purchases` as your title. You can now press the down key or press `C-n` to go to the next line. Here you are going to enter the details `I need to file a report detailing future purchases that can be made in the next 2 months. I need to run this by Bob and get his feedback before I submit it to Alice.` Once you've added your description it is time to save your TODO. Press `C-c C-c` to save it.

<TODO BEFORE SAVE PIC>

As soon as you save your new TODO entry a file will appear on the left hand side of your screen under the `org` folder. This new file is called `todos.org` and it has been created in the `org` directory, which itself is helpfully located in your home directory. Go ahead and click on the `todos.org` file in Treemacs and hit `tab`. It will expand the file in Treemacs and show you the headings. At the moment there is only one heading, the TODO you created. We want to get a closer look at this TODO, so with the `todos.org` file still highlighted, hit return. This will open the file `todos.org` in the other window and allow you to edit it.

<TREEMACS AND TODO FILE PIC>


<a id="orgc63ad87"></a>

## Start working on the TODO

You should now be back in the `todos.org` file looking at the entry you just created. Move your cursor so it is on the line with the `TODO` keyword on it. In this case, since it is our first TODO, it will be on the first line. Once you are there press `C-c C-t`. This will open the mini buffer at the bottom of Emacs and it will contain all the valid states you can change this TODO to. You will have the option of setting it to one of five states. Those are TODO, IN-PROGRESS, BLOCKED, DONE, and WON'T-DO. Since we want to start work on this task, hit the `i` key to set it to the in progress state.

As soon as you hit `i` you will notice the mini buffer popped up again. Now is your chance to add a short note about the state change you are making. Go ahead and type `I got handed this task during the morning meeting` and then press `C-c C-c` to save it. You will notice your TODO changed slightly. The first thing you should see is that you now have a `State` entry along with a timestamp and your note. This is very handy because it allows you to track your status with both notes and timestamps. Org Mode can also track the time you spend on a TODO and put it in a table, but that is too much for this tutorial.

At this point you have set your TODO to a state showing that you are currently working on it. You have added a note explaining the state change, now you can add some more details. Let's start with a new subheading called `Subtasks`. Press `C-Return` to get a new heading and then press `tab` once to make it a 2nd level heading. From here, type `Subtasks [/]` and then hit return. We are going to create an auto-incrementing list. On the line after your heading, type `- [ ] Gather notes` and press return. Two interesting things should have happened. A new check box item should have appeared on the next line, and the `Subtasks` heading should now have a count of the check box items we created. To get out of the auto checkbox mode just hit return twice. Don't worry if the count looks wrong, that will get fixed shortly.

Go ahead and fill out a few more check box items until your list looks like the image below.

Looking good so far, things are just barely starting to take shape. We now have one sub heading that tracks all of our subtasks, now we need another heading for our notes. So hit `C-Return` and then `tab` to create a new 2nd level sub heading and name this one `Notes`. Here you would add notes you took in your meeting, notes you got from an email, or notes someone handed you that you are going to summarize here. I've added some filler notes here, but you are welcome to put whatever you'd like.

Finally, once you've finished adding your notes, go back to the first line of the `Progress` subheading. With your cursor anywhere on the line that says `Gather notes` hit `C-c C-c`. Look at that! There is now an `X` next to our completed subtask and the progress counter has incremented. This is useful.


<a id="orgdf5a0c5"></a>

## Make a note


<a id="orgd261735"></a>

## Record a journal entry


<a id="orgfddfb04"></a>

## Filter the agenda


<a id="org1958fe8"></a>

## Recap

