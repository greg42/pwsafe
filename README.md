pwsafe
======

A command-line password manager written in Haskell.

It uses external programs for most of its tasks:

 * `gpg` for encryption of your password database (but you can easily adjust it
   to use `openssl` with AES and a master password)
 * `xdg-open` for interaction with your web browser
 * `xclip` for interaction with your X selection
 * `pwgen` for generation of user names and passwords
 * `vim` and `shred` if you want to edit your password database with a text editor

Currently only Unix-like systems are supported.

Any questions/comments/patches are gladly welcome!


Workflow
========

How to use pwsafe.


Adding a new entry to the password database
-------------------------------------------

Let's say you want to create an account on the website
http://www.niftyservice.com. Then you would proceed like this:

 0. Only once: setup gpg and generate a keypair.  pwsafe uses the gpg default
    keypair to decrypt and encrypt your password database.

        $ gpg --gen-key

 1. Point your web browser to http://www.niftyservice.com and navigate to the sign-up
    page where you have to enter your new login credentials like username and
    password. Don't enter anything yet.
    Usually, these pages have at least three input fields:
     * enter your desired username
     * enter your desired password
     * repeat your desired password
 2. In a terminal, call pwsafe like this:

        $ pwsafe -a http://www.niftyservice.com
        You need a passphrase to unlock the secret key for
        user: "Some Body <some.body@some.where>"
        Waiting for one selection request.

    pwsafe has generated a random username and password for you, and added it
    to the password database. At this point, pwsafe blocks and waits for a
    paste request. In X, you do that with your middle mouse button.

 3. In your web browser, middle-click on the "username" field. pwsafe pastes the
    username it has generated for you.

 4. pwsafe now waits for you to paste the password two times. Middle-click on
    the "password" field, then again in the "repeat password" field.

 5. pwsafe exits, because it's job is done for now. Fill out the rest of the
    fields in the web form to your liking.


Looking up a password in the database
-------------------------------------

If you want to log in to niftyservice again, proceed as follows.

 1. Call pwsafe like this

        $ pwsafe -q nifty
        You need a passphrase to unlock the secret key for
        user: "Some Body <some.body@some.where>"
        Waiting for one selection request.

    The term *nifty* must match exactly one entry in your password database. If
    you have two entries, one for niftyservice and one for niftycatpictures,
    pwsafe exits with an error. Make the term long enough to be unambiguous.

    At this point, pwsafe has opened the website that corresponds to the entry
    in your default web browser. To configure your default web browser, consult
    the documentation of xdg-open and update-alternatives. If you do not want
    pwsafe to open your browser, please supply the -d option.

 2. Switch to your browser window and navigate to the login page.

 3. Middle-click on the "username" field.

 4. Middle-click on the "password" field.

 5. pwsafe exits, and you can proceed with the login.


Using multiple accounts per service
-----------------------------------

pwsafe allows you to add more than just one user account per service. In order
to do so, you have to supply the -A option when adding a second user account.
When you query the database, pwsafe will either copy the credentials into the
paste buffer (if there is only one account for the respective service) or write
a list of known usernames to stdout and exit with code 2. You can supply the
--user parameter when querying, so that pwsafe knows which entry in the database
you would like to have. 


Changing or deleting an entry in the database
---------------------------------------------

pwsafe does not have commands to edit entries in your database. Instead, it
fires up vim with the complete decrypted database for you to edit it. pwsafe
decrypts your database to a temporary file, loads this file in vim with
sensible defaults (no backupfile, no vimfile, no swapfile, etc). Once you are
done editing, pwsafe encrypts the file again and calls `shred` to securely
delete the decrypted copy. If you are using an SSD, shred is useless, so you
better be using hardware encryption.

Currently, only vim is supported for database editing.

An entry in the database has a symbolic name and three fields: user, url and
password.

    [www.niftyservice.com]
    user=rwDJEs5J
    password=XArG9R4QBDwR7ceCVjyV
    url=http://www.niftyservice.com
    user_1=another_user
    password=1=BLABLA
    url=http://www.niftyservice.com

Only the "name" (i.e., section name) and "password" fields are required. "user"
and "url" are optional.  If the url field is not present, pwsafe will not invoke
your web browser on query.  If the user field is not present, pwsafe will only
provide the password to paste on query.


Command line options
====================

    Usage: pwsafe [OPTION]...

                 --help           display this help and exit
      -a URL     --add=URL        add a new entry to the database; the password is
                                  always automatically generated; the username is
                                  generated unless --user is specified
      -q TERM    --query=TERM     lookup a password, the term must match exactly one
                                  entry
      -l[TERM]   --list[=TERM]    list all entries matching the given term
      -e         --edit           invoke vim to edit the database using sensible
                                  defaults (no backup, no swapfile etc)
                 --dump           dump database to stdout
                 --lock           acquire write lock for database
                 --unlock         release write lock for database
                 --dbfile=FILE    file where passwords are stored;
                                  defaults to ~/.pwsafe/db
                 --user=USER      specify a username to be used for an entry;
                                  this option is to be used with --add or -q
      -n NUMBER                   copy password n times to clipboard;
                                  defaults to 1
                 --password-only  only copy password to clipboard
      -g GPG Option               add a GPG option (repeat to add multiple)
      -d                          don't open a browser when using -q
      -A                          force add a new account

Development
===========

pwsafe is primarily here to serve my needs.  If we can extend it in a way that
makes it more useful for you and still serves my needs, even better!

Make sure that the test suite passes with your changes and add tests for new
code.

### Running the tests

First make sure that you have the latest version of `cabal-install`:

    $ cabal update && cabal install cabal-install

Make sure that `~/.cabal/bin/` is on your `PATH`.

Run the tests:

    $ cabal install --enable-tests --dependencies-only
    $ cabal test

### Running the tests during development

During development you want to run the test with GHCi.  This is much faster and
provides better feedback:

    $ chmod og-w .ghci
    $ ghci test/Spec.hs
    *Main> :main

After making changes, run:

    *Main> :reload
    *Main> :main
