# The Komodo project

## Summary

This is the official git repository for the Komodo project.

Komodo is an open source visual tool that enables rapid, model-driven definition, integration, management and testing of data services without programming using the Teiid runtime framework. With Komodo, not only do you create source data models and map your sources to target formats using a visual tool, but you can also:

*	create a virtual database (or VDB) containing your models which you deploy to a Teiid instance and then access your data.
*	resolve semantic differences
*	create virtual data structures at a physical or logical level
*	use declarative interfaces to integrate, aggregate, and transform the data on its way from source to a target format which is compatible and optimized for consumption by your applications

You can use Komodo to integrate multiple sources, and access them using the common data access standards:

*	Web Services / SOAP / XML
*	JDBC / SQL
*	ODBC / SQL

Komodo represents the next generation of modelling tool for Teiid, building on the success of Teiid Designer.

For more information on Komodo, visit the Teiid Designer project's website at [http://www.jboss.org/teiiddesigner/](http://www.jboss.org/teiiddesigner/)
or follow us on our [blog](http://teiid.blogspot.com/) or on [Twitter](https://twitter.com/teiiddesigner). Or hop into our [IRC chat room](http://www.jboss.org/teiiddesigner/chat)
and talk our community of contributors and users.

## Get the code

The easiest way to get started with the code is to [create your own fork](http://help.github.com/forking/) of this repository, and then clone your fork:

	$ git clone git@github.com:<you>/komodo.git
	$ cd komodo
	$ git remote add upstream git://github.com/Teiid-Designer/komodo.git
	
At any time, you can pull changes from the upstream and merge them onto your master:

	$ git checkout master               # switches to the 'master' branch
	$ git pull upstream master          # fetches all 'upstream' changes and merges 'upstream/master' onto your 'master' branch
	$ git push origin                   # pushes all the updates to your fork, which should be in-sync with 'upstream'

The general idea is to keep your 'master' branch in-sync with the 'upstream/master'.

## Build the Code

To build the komodo code, cd into your komodo local repo, then use the build script

	$ cd komodo                         # switches to the 'master' branch
	$ ./build.sh                        # build the code (you can add '-s' to skip unit tests)

To setup a development environment in Eclipse, please refer to this [article](https://developer.jboss.org/docs/DOC-53242).

## Contribute fixes and features

Komodo is open source, and we welcome anybody that wants to participate and contribute!

If you want to fix a bug or make any changes, please log an issue in the [Teiid Designer JIRA](https://issues.jboss.org/browse/TEIIDDES) describing the bug or new feature. Then we highly recommend making the changes on a topic branch named with the JIRA issue number. For example, this command creates
a branch for the TEIIDDES-1234 issue:

	$ git checkout -b teiddes-1234

After you're happy with your changes and a full build (with unit tests) runs successfully, commit your changes on your topic branch
(using [really good comments](http://community.jboss.org/wiki/TeiidDesignerDevelopmentGuidelines#Commits)). Then it's time to check for
and pull any recent changes that were made in the official repository:

	$ git checkout master               # switches to the 'master' branch
	$ git pull upstream master          # fetches all 'upstream' changes and merges 'upstream/master' onto your 'master' branch
	$ git checkout mode-1234            # switches to your topic branch
	$ git rebase master                 # reapplies your changes on top of the latest in master
	                                      (i.e., the latest from master will be the new base for your changes)

If the pull grabbed a lot of changes, you should rerun your build to make sure your changes are still good.
You can then either [create patches](http://progit.org/book/ch5-2.html) (one file per commit, saved in `~/teiddes-1234`) with 

	$ git format-patch -M -o ~/teiddes-1234 orgin/master

and upload them to the JIRA issue, or you can push your topic branch and its changes into your public fork repository

	$ git push origin teiddes-1234         # pushes your topic branch into your public fork of Komodo

and [generate a pull-request](http://help.github.com/pull-requests/) for your changes. 

We prefer pull-requests, because we can review the proposed changes, comment on them,
discuss them with you, and likely merge the changes right into the official repository.

