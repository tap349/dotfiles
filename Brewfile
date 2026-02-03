# https://github.com/Homebrew/homebrew-bundle

# - some cask packages and App Store applications ask for password
# - see comments before package or application for postinstallation setup
# - see `brew services` on how to manage services for supported forumalae

#-------------------------------------------------------------------------------
# Taps (third-party repositories)
#
# homebrew/bundle and homebrew/services are automatically installed (tapped)
# when corresponding commands (`brew bundle` or `brew services`) are run
#-------------------------------------------------------------------------------

tap 'beeftornado/rmtree'
tap 'homebrew/cask'
tap 'homebrew/cask-fonts'
tap 'homebrew/cask-versions'
# https://github.com/osxfuse/osxfuse/issues/818#issuecomment-985739918
tap 'gromgit/homebrew-fuse'

#-------------------------------------------------------------------------------
# Homebrew
#-------------------------------------------------------------------------------

brew 'asdf'
# After installation:
# - `aws configure`
brew 'awscli'
# `cat` on steroids
brew 'bat'
#brew 'exiftool'
brew 'ffmpeg'
brew 'git'
brew 'gpg'
brew 'htop'
brew 'imagemagick'
brew 'jq'
# For psql and pg_dump
#brew 'libpq'
# Used as viewer for html files in mc
#brew 'lynx'
brew 'macvim'
brew 'mas'
brew 'mc'
brew 'mpv'
brew 'node'
brew 'ntfs-3g-mac'
# `foreman` alternative
#brew 'overmind'
brew 'p7zip'
#brew 'pg_top'
# Use docker-compose.yml instead, install libpq for psql and pg_dump
#brew 'postgresql', restart_service: :changed
brew 'ripgrep'
brew 'ssh-copy-id'
# For `brew install font-inconsolata-lgc`
brew 'svn'
brew 'tokei'
brew 'tree'
brew 'wget'
brew 'yt-dlp'
# Make it a login shell: `chsh -s /bin/zsh` (all available shells are listed
# in /etc/shells, current shell can be printed with `echo $0` command)
brew 'zsh'

#-------------------------------------------------------------------------------
# Cask
#-------------------------------------------------------------------------------

cask_args appdir: '/Applications'

# for mvrviewer-4.6.1213.air
# https://community.adobe.com/t5/air/adobe-air-framework-is-damaged-and-can-t-be-opened/m-p/10799074#M55885
#
# After installation:
# ```
# sudo xattr -r -d com.apple.quarantine /Library/Frameworks/Adobe\ AIR.framework
# ```
#
# `Download failed` error
#cask 'adobe-air'
cask 'anki'
cask 'chatgpt'
cask 'flux-app'
# For iTerm2
cask 'font-inconsolata-lgc'
cask 'font-ubuntu'
cask 'google-chrome'
cask 'iterm2'
cask 'macfuse'
cask 'ngrok'
cask 'obsidian'
cask 'steam'
cask 'todoist-app'
cask 'tomighty'
cask 'transmission'
cask 'zoom'

#-------------------------------------------------------------------------------
# App Store
#
# mas search Trello
#-------------------------------------------------------------------------------

#mas 'Cloud Mail.Ru', id: 893068358
mas 'Dashlane – Password Manager', id: 517914548
#mas 'Dual N-Back - Train of Thought', id: 1104323582
mas 'Ghostery Lite', id: 1436953057
mas 'Happ - Proxy Utility Plus', id: 6746188973
mas 'Sip', id: 507257563
mas 'Slack for Desktop', id: 803453959
mas 'Telegram Desktop', id: 946399090
mas 'WhatsApp Messenger', id: 310633997

#-------------------------------------------------------------------------------
# LSP
#-------------------------------------------------------------------------------

brew 'gopls'

#-------------------------------------------------------------------------------
# inDrive
#-------------------------------------------------------------------------------

tap 'aws/tap'
tap 'jimeh/emacs-builds'
tap 'telepresenceio/telepresence'

brew 'bfg'
brew 'cockroach-sql'
brew 'eks-node-viewer'
brew 'fd'
brew 'fzf'
brew 'gh'
# For pprof
brew 'graphviz'
brew 'helm'
brew 'ipcalc'
brew 'k9s'
brew 'kubectx'
brew 'mysql-client'
brew 'ncdu'
brew 'telepresence-oss'
brew 'temporal'

brew 'colima'
brew 'docker'
# docker-buildx is a Docker plugin. For Docker to find this plugin, symlink it:
# ```
# mkdir -p ~/.docker/cli-plugins
# ln -sfn /opt/homebrew/opt/docker-buildx/bin/docker-buildx ~/.docker/cli-plugins/docker-buildx
# ```
brew 'docker-buildx'
brew 'docker-compose'
# For colima
brew 'qemu'

cask '1password'
cask 'cloudflare-warp'
cask 'drawio'
#cask 'emacs-app-monthly'
cask 'emacs-app@nightly'
cask 'figma'
cask 'gcloud-cli'
cask 'intellij-idea'
cask 'microsoft-edge'
cask 'miro'
cask 'openlens'
cask 'postman'
#cask 'protonvpn'
cask 'pgadmin4'
cask 'yaak'

#mas 'Google Tasks Client - To-Do', id: 1498581975
