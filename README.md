
# Table of Contents

1.  [Welcome New Org Mode Users](#org76feb9a)
2.  [What Is This And Why Do I Need It?](#org011707a)
3.  [Installation](#org291fa6e)
4.  [Overview](#orgea2c335)
5.  [I've Installed It, Now What?](#orgc236f83)

****Quick Start Guide****
For more details see below, but this is the minimum you need to make this work.

1.  Backup your `.emacs` file if you have one (`mv ~/.emacs ~/.emacs.bak`)
2.  Download the `.emacs` file from this repo
3.  Move it to your home directory (`mv ~/Downloads/.emacs ~/`)
4.  You are done, now restart `emacs`


<a id="org76feb9a"></a>

# Welcome New Org Mode Users

So you want to use Org Mode, but you don't have 2 years to devote to teaching yourself Lisp? No problem. This repo was created just for you. Here you will find a simple solution that requires no additional configuration on your part to work. It is ideal for users who are completely new to both Emacs and Org Mode. Simply drop this config file into your home directory, restart Emacs, and you are ready to go.

If you came here looking for a more in depth explanation of some of Org Mode's core features, please check out the very detailed write up I did in my [Emacs Org Mode Tutorial](https://github.com/james-stoup/emacs-org-mode-tutorial/). You don't need to read that document (it is huge and can be overwhelming) as it is designed for people who want to really customize their Org Mode configurations. However, it does provide a lot of useful explanations with screenshots, so maybe come back to it once you've gotten some more experience with Org Mode.


<a id="org011707a"></a>

# What Is This And Why Do I Need It?

If I had to describe the entire Emacs ecosystem with only one word, it would be 'overwhelming'. A lot of Emacs related documentation assumes you already know quite a bit about the system and the documentation that is geared for new users is often so densely packed with information that it can be difficult to orient yourself amid the flood of knowledge. In short, it can be overwhelming. This is especially true for new users who don't care about "THE POWER OF EMACS" but instead just want to try out Org Mode.

The good news is that Org Mode "just works" right out of the box. The bad news is that the defaults are not great and without a lot of customization, it isn't clear **at all** that any of this stuff is actually useful. I discovered this first hand when I helped a new user get started in Org Mode. My org configuration is hundreds of lines long. So when this new user saw my version of Org Mode and compared it to their version of Org Mode, they were rather disappointed. Where was the magic? Why did mine look fancy and theirs look so plain? And what did they have to do to get from where they were to where I was. I could have told them to just play with it for a year or two and then they would eventually get an idea of what they needed, but I knew if I did that they would just find another solution. What to do?

I sent them the link I put above so they could check out my tutorial on designing your Org Mode configuration from scratch. But they didn't want that. In fact, they had no idea what they wanted. The only thing they were sure of was that they wanted it to be functional now. Not in a year, not after they had learned lisp, right now. So what I did was massively pare down my own Emacs configuration until it was a lean setup geared entirely towards Org Mode. No code completion, language servers, linters, git integration, or anything else that was directly related to Org Mode. Then I further cleaned it up to make it as easy to use as possible. Finally, I helped them install this new config and gave them some minimal instructions on how to use it. Guess what? They loved it. Based on their response I realized that there were probably a lot of people in the same boat who would appreciate a little help getting started with Org Mode. So if that is you, I hope this helps you.


<a id="org291fa6e"></a>

# Installation

To make your version of Emacs and Org Mode look like the screenshots below, download the `.emacs` file in this repo and either copy it into your existing `.emacs` file or overwrite it entirely. Then restart Emacs, open (or create) an Org file and you should a much nicer looking version of Org. To verify Org looks different you can download the `org-better-defaults.org` file in this repo and open it.


<a id="orgea2c335"></a>

# Overview

This configuration has two core parts to it. The first part deals with improving how Org Mode looks and functions at a basic level. The second core part of this configuration is the default values provided. For more details, look over the `org-better-defaults.org` file in this repo.

Here is a list of most of the primary enhancements provided:

****Improvements\*****

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


<a id="orgc236f83"></a>

# I've Installed It, Now What?

If you've already installed the config file and restarted Emacs, then it is time to begin your journey into productivity! 

