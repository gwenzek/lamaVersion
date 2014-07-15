This project aims at simplifing the cycle of "code upgrade, experiment, interpretation",
and to allow the repeatability of experiments.

To run lamaVersion simply put all your script (*.sh files) in a "experiment" directory,
create a "result" directory and lauch the lamaVersion jar with scala, with the following args: "your git directory" "the experiment directory" "the result directory"

Example:

    scala lamaVersion.jar myProject experiments results

LamaVersion will clone your git project, and run the given experiments on all commits of your project. This will allow you to spot which commit was responsible for the drop of your performances.

You can add the following comments in your script:

    # BEGIN: Thu Jun 26 15:40:09 2014 +0200
    # END: Thu Jul 10 12:20:40 2014 +0200

to specify the date range you whish to apply your tests on (those dates are included).
This date format is the one used by git so you can copy paste date from the ouput of "git log".

(Note: currently you have to put the spaces after the # and the :)

Often your are not only interested in the standard output of the script but you are also interested by some files dumped by your programm. To copy them you can add a line in your script like: 
    
    # GET: HelloWorld.txt

To exclude a specific commit you can add an EXCLUDE line :
    
    # EXCLUDE: e19dfbf4

You can use either the short or the long hash here.