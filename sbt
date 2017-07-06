#!/bin/bash

get_java_cmd() {
  if [[ -n "$JAVA_HOME" ]] && [[ -x "$JAVA_HOME/bin/java" ]]; then
    echo "$JAVA_HOME/bin/java"
  else
    echo "java"
  fi
}

# by default we should be in the correct project dir, but when run from Finder on Mac, the cwd is wrong
move_to_project_dir() {
  if [[ "$(uname)" == "Darwin" ]] && [[ "$HOME" == "$PWD" ]]; then
    cd $(dirname $0)
  fi
}

move_to_project_dir

SBT_LAUNCHER="$(dirname $0)/sbt-launch.jar"

SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled"

# todo: check java cmd

# todo: help text

$(get_java_cmd) ${SBT_OPTS} -jar ${SBT_LAUNCHER} "$@"
