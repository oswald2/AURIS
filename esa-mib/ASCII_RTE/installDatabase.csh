#!/bin/csh
#----------------------------------------------------------------------------
#
# (C) 2002      European Space Agency
#               European Space Operations Centre
#               Darmstadt, Germany
#
#----------------------------------------------------------------------------
#
# 
# file:    installDatabase.csh script
#
# purpose: This script installs a database in directory
#          $HOME/db_inst_temp/ASCII to $HOME/ASCII. The old release
#          is moved to a temporary directory and deleted on success.
#
#          This script must be included as part of a database release.
#
#      date          author         purpose of change
#    ------------+-----------------+---------------------
#     15-04-2002   D.Chapman        Initial version
#     28-04-2002   D.Chapman        Updated mechanism
#
#----------------------------------------------------------------------------

cd $HOME
 
set new_inst_dir=$HOME/db_inst_temp

# Check that new ASCII directory exists
if (! -r $new_inst_dir/ASCII) then
  echo Error finding new release
  exit(1)
endif

# if temp dir already exists this could be from an installation that
# failed, and therefore contains the last valid database.
if (-r $HOME/ASCII_old) then
  echo Directory ASCII_old already exists.
  echo May contain last valid release.
  exit(1)
endif

# move current database to temp dir
if (-r $HOME/ASCII) then
  /usr/bin/mv $HOME/ASCII $HOME/ASCII_old
  if ($status != 0) then
    echo Error moving current ASCII directory to ASCII_old
    exit(1)
  endif
endif

# move new ASCII directory to current
mv $new_inst_dir/ASCII $HOME
if ($status != 0) then
  echo Error copying new database to current
  exit(1)
endif

# remove old temporary directory
if (-r $HOME/ASCII_old) then
  /usr/bin/rm -fr $HOME/ASCII_old
  if ($status != 0) then
    echo Error deleting ASCII_old, but release installed
  endif
endif

# remove any old distribution file in $HOME
if (-r $HOME/ASCII.tar) then
  /usr/bin/rm -f ASCII.tar
  if ($status != 0) then
    echo Error deleting $HOME/ASCII.tar, but release installed
  endif
endif

# move new distribution file to $HOME/ASCII.tar
/usr/bin/mv $new_inst_dir/*.tar $HOME/ASCII.tar
if ($status != 0) then
  echo Error moving tar file to $HOME/ASCII.tar, but release installed
else
  chmod 664 $HOME/ASCII.tar
  if ($status != 0) then
    echo Error chmod'ing $HOME/ASCII.tar, but release installed
  endif
endif

# remove installation directory
/usr/bin/rm -fr $new_inst_dir
if ($status != 0) then
  echo Error deleting $new_inst_dir, but release installed
endif

# Create new_db_version file so that we know that a db import is required
touch $HOME/new_db_version
if ($status != 0) then
  echo Error creating new_db_version file, but release installed
endif 

# Set priviledge so that ops account can delete new_db_version
chmod 664 $HOME/new_db_version
if ($status != 0) then
  echo Error chmod'ing new_db_version file, but release installed
endif 

echo Success
    
exit 0
