#! /bin/bash -e

function check_status {
  if [[ `git status --porcelain | wc -l` == "0" ]]; then
      return 0
  else
      return 1
  fi
}

if check_status; then
    echo 'deploying to heroku...'

    BRANCH=`git rev-parse --abbrev-ref HEAD`
    git checkout deploy
    git add --force dist/ dist/node_modules static/ static/js/
    git commit -m 'deploying'
    git push heroku deploy:master

    echo "switching back to ${BRANCH}..."
    git checkout "$BRANCH"
else
    echo 'Your working tree is dirty or there are untracked files.'
    echo 'Commit or stash your changes first.'
    echo '`git status --porcelain` should not print anything.'
fi
