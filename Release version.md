# Steps to take when releasing new version
This is just a checklist for myself.

1. Update ChangeLog.md
2. Bump version in package.yaml
3. Run `stack build`
4. Run `stack sdist` and upload to hackage
5. Run `snapcraft` and `snapcraft upload --release=stable hascard_a.b.c.d_amd64.snap`
6. Commit with version tag and push to github
```
git add .....
git commit -m .....
git tag va.b.c.d
git push --tags origin
git push origin master
```
7. Wait till Travis CI completes, and then download the macos release
8. Run `shasum -a 256 hascard-va.b.c.d-osx.tar.gz`
9. Update Yvee1/homebrew-tools/hascard.rb url and sha256