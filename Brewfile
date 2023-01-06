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

# Remove formula with its unused dependencies:
# `brew rmtree mpv`
tap 'beeftornado/rmtree'
tap 'homebrew/cask'
tap 'homebrew/cask-fonts'
tap 'homebrew/cask-versions'
# https://github.com/osxfuse/osxfuse/issues/818#issuecomment-985739918
tap 'gromgit/homebrew-fuse'

#-------------------------------------------------------------------------------
# Homebrew
#-------------------------------------------------------------------------------

#brew 'ansible'
# After installation:
# - `aws configure`
brew 'awscli'
# `cat` on steroids
brew 'bat'
#brew 'bfg'
# Standard `mount -t bind` command doesn't work
# No bottle for Apple Silicon
#brew 'bindfs'
#brew 'ccze'
#brew 'boost'
# Install manually by downloading dmg file
#brew 'cgoban'
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
brew 'lynx'
brew 'macvim'
brew 'mas'
brew 'mc'
brew 'minikube'
# For React Native and coc.nvim
# (same as nodejs)
brew 'node'
brew 'ntfs-3g-mac'
#brew 'overmind'
brew 'p7zip'
#brew 'pg_top'
# Use docker-compose.yml instead, install libpq for psql and pg_dump
#brew 'postgresql', restart_service: :changed
brew 'ripgrep'
# For command-t
brew 'ruby'
brew 'ssh-copy-id'
# For `brew install font-inconsolata-lgc`
brew 'svn'
brew 'tmux'
brew 'tokei'
brew 'tree'
brew 'wget'
brew 'yarn'
brew 'youtube-dl'
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
# - `sudo xattr -r -d com.apple.quarantine /Library/Frameworks/Adobe\ AIR.framework`
#
# `Download failed` error
#cask 'adobe-air'
cask 'anki'
# Run docker app after installation
# (otherwise `docker` command might be not available in terminal)
cask 'docker'
#cask 'figma'
cask 'firefox'
cask 'flux'
# For iTerm2
cask 'font-inconsolata-lgc'
cask 'google-chrome'
cask 'gopanda'
cask 'insomnia'
cask 'iterm2'
cask 'lens'
cask 'macfuse'
cask 'mpv'
#cask 'ngrok'
cask 'notion'
#cask 'openemu'
#cask 'opera'
#cask 'skype'
#cask 'steam'
cask 'tomighty'
cask 'transmission'
#cask 'vagrant'
#cask 'virtualbox'
#cask 'virtualbox-extension-pack'
# `Download failed` error
#cask 'whatsapp'
cask 'zoom'

#-------------------------------------------------------------------------------
# App Store
#
# mas search Trello
#-------------------------------------------------------------------------------

mas 'AdBlock', id: 1402042596
mas 'Cloud Mail.Ru', id: 893068358
#mas 'Dual N-Back - Train of Thought', id: 1104323582
#mas 'Evernote', id: 406056744
# - Preferences:
#   - Window Mover:
#     - Move: <S-M> + one finger (+ Left Mouse Button for mouse)
#     - Resize: <S-M> + two fingers (+ Right Mouse Button for mouse)
#   - Layouts:
#     - Maximize: <S-M-CR>
#   - Preferences:
#     - [x] Autostart on system login
#     - [ ] Show icon in menu bar
#     - [ ] Show icon in Dock
#mas 'Flexiglass', id: 426410278
#mas 'FTP Server', id: 987045856
mas 'Ghostery Lite', id: 1436953057
#mas 'Jira Cloud by Atlassian', id: 1475897096
#mas 'Marked 2', id: 890031187
#mas 'Microsoft Remote Desktop', id: 715768417
# - Preferences:
#   - General:
#     - After upload:
#       - [ ] Open in browser
#     - [x] Launch at login
#   - Hotkeys:
#     - Capture area: <M-1> (<M-D-5> is used by default)
mas 'Monosnap', id: 540348655
# - Preferences:
#   - Shortcuts:
#     - General -> Show Magnifier: <M-2>
mas 'Sip', id: 507257563
mas 'Slack for Desktop', id: 803453959
mas 'Telegram Desktop', id: 946399090
#mas 'iA Writer', id: 775737590

#-------------------------------------------------------------------------------
# inDriver
#-------------------------------------------------------------------------------

tap 'jimeh/emacs-builds'

brew 'argocd'
brew 'cilium-cli'
brew 'cockroachdb/tap/cockroach'
brew 'emacs-app-good'
brew 'fd'
brew 'fzf'
brew 'gradle'
brew 'hashicorp/tap/terraform'
brew 'helm'
brew 'hubble'
brew 'ipcalc'
brew 'k9s'
brew 'kubectx'
brew 'ncdu'
# JDK19 is not supported by gradle
brew 'openjdk@17'

cask 'drawio'
cask 'postman'
cask 'protonvpn'

mas 'WireGuard', id: 1451685025
