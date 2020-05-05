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

# for wuzz
tap 'ZloeSabo/homebrew-nettools'
# remove formula with its unused dependencies:
# `brew rmtree mpv`
tap 'beeftornado/rmtree'
tap 'homebrew/cask'
tap 'homebrew/cask-fonts'
tap 'puma/puma'
tap 'universal-ctags/universal-ctags'

#-------------------------------------------------------------------------------
# Homebrew
#-------------------------------------------------------------------------------

# for minisklad
brew 'FreeTDS'
brew 'ansible'
# after installation:
# - `aws configure`
brew 'awscli'
# cat on steroids
brew 'bat'
brew 'bfg'
# standard `mount -t bind` command doesn't work
brew 'bindfs'
brew 'ccze'
#brew 'chromedriver'
brew 'boost'
# install manually by downloading dmg file
#brew 'cgoban'
#brew 'djview4'
brew 'djvu2pdf'
brew 'exiftool'
# use docker-compose.yml instead
#brew 'elasticsearch', restart_service: :changed
#brew 'fish'
# for ~/scripts/fontpatcher
brew 'fontforge'
# for shikimori
#brew 'geoip'
brew 'git'
brew 'gpg'
# to install Gradle Wrapper
brew 'gradle'
# use overmind instead
#brew 'honcho'
brew 'htop'
brew 'imagemagick'
brew 'jq'
# use docker-compose.yml instead
#brew 'kibana'
# for psql and pg_dump
#
# after installation:
# - add _/usr/local/opt/libpq/bin_ to PATH
brew 'libpq'
# provides rsvg-convert utility to convert svg to png
brew 'librsvg'
# used as viewer for html files in mc
brew 'lynx'
# - install vim-plug:
#   `curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim`
# - install plugins:
#   :PlugInstall
#
# - Preferences:
#   - General:
#     - After last window closes: Quit MacVim
#   - Advanced:
#     - [ ] Use Core Text renderer (it crashes MacVim since from 8.1-151_1)
#     - [ ] Draw marked text inline
brew 'macvim'
brew 'mas'
brew 'memcached', restart_service: :changed
brew 'mc'
# for React Native
# (same as nodejs)
brew 'node'
brew 'octave'
brew 'overmind'
brew 'p7zip'
# not used
#brew 'pg_top'
# use docker-compose.yml instead
# install libpq for psql and pg_dump
#brew 'postgresql', restart_service: :changed
# after installation:
# - `sudo puma-dev -setup`
# - `puma-dev -install`
# - add symlinks to _~/.puma-dev/_
# - add `gem 'puma'` to _Gemfile_ of all symlinked applications
#   (for development and test groups only)
brew 'puma-dev'
# - it's much easier to install pow manually:
#   `curl get.pow.cx | sh`
# - create symlinks for all required projects:
#   `ln -s ~/dev/reenter_builder ~/.pow`
#brew 'pow'
brew 'rabbitmq'
# asdf is used instead now
#brew 'rbenv'
brew 'redis', restart_service: :changed
brew 'ripgrep'
brew 'ssh-copy-id'
# https://taskwarrior.org/download
brew 'task'
brew 'taskd'
brew 'tasksh'
#brew 'tig'
brew 'tmux'
brew 'tokei'
brew 'tree'
# universal-ctags handle Ruby code and have built-in Elixir support -
# no need for external ctags file
brew 'universal-ctags', args: ['HEAD']
brew 'unrar'
# for React Native
brew 'watchman'
brew 'wget'
brew 'wuzz'
brew 'yarn'
# make it a login shell: `chsh -s /bin/zsh` (all available shells are listed
# in /etc/shells, current shell can be printed with `echo $0` command)
brew 'zsh'

#-------------------------------------------------------------------------------
# Homebrew-Cask
#-------------------------------------------------------------------------------

cask_args appdir: '/Applications'

# for mvrviewer-4.6.1213.air
# https://community.adobe.com/t5/air/adobe-air-framework-is-damaged-and-can-t-be-opened/m-p/10799074#M55885
#
# after installation:
# - `sudo xattr -r -d com.apple.quarantine /Library/Frameworks/Adobe\ AIR.framework`
cask 'adobe-air'
# for android-sdk
# http://blog.tap349.com/react-native/android/2017/05/24/react-native-android/#install-java-8
cask 'adoptopenjdk8'
# for Raspeberry Pi
#cask 'applepi-baker'
cask 'calibre'
# install required gems (knife-zero, chef, berkshelf) using Gemfile
#cask 'chefdk'
# - Preferences:
#   - General:
#     - Startup:
#       - [ ] Launch Dash at login
#       - [x] Restore all open tabs from last session
#     - Global search shortcut: F1
#     - Behavior:
#       - [x] Show dock icon
#       - [ ] Show menu bar icon
#       - [ ] Dismiss main window when I activate a different app
#   - Web Search:
#     - [ ] Google
#     - [ ] DuckDuckGo
#     - [ ] Stack Overflow
#cask 'dash'
# - Preferences:
#   - Date & Time Format: E HH:mm
#   - [x] Show Icon
#   - [x] Launch Day-O at login
#cask 'day-o'
# run docker app after installation
# (otherwise `docker` command might be not available in terminal)
#
# - System Preferences:
#   - Users & Groups -> Login Items: add (don't hide)
cask 'docker'
cask 'firefox'
#cask 'font-fontawesome'
#cask 'font-cousine'
#cask 'font-d2coding'
#cask 'font-droid-sans-mono'
#cask 'font-fira-mono'
#cask 'font-inconsolata-dz'
# for iTerm2
cask 'font-inconsolata-lgc'
#cask 'font-iosevka'
cask 'genymotion'
# app preferences (bookmarks bar, extensions, etc.) are synchronized if
# you sign in to Chrome
#
# on first run:
#  - [x] Set Google Chrome as your default browser
#  - [ ] Help make Google Chrome better by automatically sending usage
#        statistics and crash reports to Google
#  - Sign in to Chrome (page is opened automatically)
#
# - System Preferences:
#   - Users & Groups -> Login Items: remove
# - Preferences:
#   - Settings:
#     - On startup: Continue where you left off
#     - Content Settings...:
#       - Flash:
#         - [x] Ask first (recommended)
#   - Extensions (allow all of them in incognito):
#     + AdBlock (Hide in Chrome Menu)
#       - [ ] Show number of ads blocked on AdBlock button
#     - Batch Media Saver from Instagram™
#     + Blank New Tab (Hide in Chrome Menu)
#     + Browsec VPN
#     + Dashlane (select Desktop mode, can be added on first Dashlane run)
#     - Data Saver
#     + Downloader for Instagram™ + Direct Message (Hide in Chrome Menu)
#     + Ghostery (Hide in Chrome Menu):
#       - Global Blocking:
#         - Block All (except Comments)
#       - Notifications:
#         - [ ] Show tracker count badge on the Ghostery icon in browser toolbar
#       - Purple Box:
#         - [ ] Show the purple box in the corner of my browser
#     + Google Docs Offline (installed by default)
#     - GoUniverse (Hide in Chrome Menu)
#     - Grammarly for Chrome (Hide in Chrome Menu):
#       - This can read and change site data:
#         - [x] When you click the extension
#     - JSON Formatter (Hide in Chrome Menu)
#     + JSON Viewer (Hide in Chrome Menu)
#       - Theme: mdn-like
#       - Structure:
#         - "lineNumbers": false
#     + IG Stories for Instagram (Hide in Chrome Menu)
#     + Noisli
#     - Proxy SwitchyOmega (configure proxy per site)
#     - Quick Javascript Switcher (Hide in Chrome Menu)
#     - React Developer Tools (Hide in Chrome Menu)
#     - Scrum for Trello (Hide in Chrome Menu)
#     + Session Buddy
#     - Tag Assistant (by Google) (enabled per tab)
#     + The Great Suspender
#       - Settings:
#         - [x] Never suspend pinned tabs
#         - [x] Never suspend tabs that contain unsaved form inputs
#         - [x] Never suspend tabs that are playing audio
#         - [x] Never suspend active tab in each window
#         - [x] Never suspend tabs when offline
#     + uBlock Origin (Hide in Chrome Menu):
#       - Settings:
#         - [ ] Show the number of blocked requests on the icon
#     - Viewport Dimensions (Hide in Chrome Menu)
#     + Google Docs (installed by default)
#     + Google Sheets (installed by default)
#     - Google Slides (installed by default)
#   - Theme:
#     - Black Carbon + silver meta
#     - Carbon Light
#     - or else search using keywords:
#       - flat
#       - brushed
#       - graphite
#       - macos
#       - material
#   - chrome://plugins:
#     - disable Adobe Flash Player
# - bookmarks bar:
#   - pin! (https://pinboard.in/howto/)
# - developer tools:
#   - Dock to bottom
cask 'google-chrome'
cask 'gopanda'
cask 'insomnia'
# - Preferences:
#   - General -> Preferences:
#     - [x] Load preferences from a custom folder or URL:
#       /Users/tap/.dotfiles/.config/iterm2/
#       (this directory should contain com.googlecode.iterm2.plist settings file
#       exported using 'Save Current Settings to Folder' button below).
#       or else copy ~/Library/Preferences/com.googlecode.iterm2.plist -
#       in both cases iTerm will be fully configured (restart is required)
#     - [x] Save changes to folder when iTerm2 quits
#
# see also http://tap349.github.io/iterm/rails/2016/05/03/iTerm/
# (all these settings are already included in exported settings file)
cask 'iterm2'
# - System Preferences:
#   - Users & Groups -> Login Items: remove
#cask 'keybase'
cask 'krisp'
# go app with AI
#cask 'leela'
cask 'mpv'
cask 'ngrok'
# OBS quits unexpectedly after start
#cask 'obs'
cask 'openemu'
cask 'opera'
cask 'pgadmin4'
cask 'phantomjs'
cask 'psequel'
cask 'react-native-debugger'
cask 'skype'
# - Preferences:
#   - Menubar: Temperature only
#   - [x] Check for updates on startup
#   - [x] Autostart smcFanControl after login
cask 'smcfancontrol'
# - System Preferences:
#   - Users & Groups -> Login Items: remove
cask 'steam'
cask 'transmission'
# for Raspeberry Pi (?)
#cask 'unetbootin'
cask 'whatsapp'
cask 'zoomus'

#-------------------------------------------------------------------------------
# App Store
#
# mas search Trello
#-------------------------------------------------------------------------------

# - link to dropbox account and sync (Replace Local Data)
# - top menu:
#   - Edit:
#     - Spelling and Grammar:
#       - [ ] Automatic spell checking
#       - [ ] Automatic spell correction
# - Preferences:
#   - General:
#     - Quick Entry: <C-S-a>
#     - [x] GTD Inbox
#     - [x] Launch 2Do Helper on System Startup
#   - Appearance:
#     - Font Size: Big
#     - [ ] Show notes under tasks
#   - Sync:
#     - Setup:
#       - Link Dropbox Account
mas '2Do', id: 477670270
mas 'ABBYY Lingvo European', id: 467622356
mas 'AdBlock', id: 1402042596
# on first run:
# - select folders to synchronize (books/, education/, videos/)
# - Settings:
#   - General (tab):
#     - Select a folder to synchronize with the Cloud: ~/Documents
#       (Cloud Mail.Ru subfolder will be created automatically)
#     - [x] Start application on system startup
#   - Screenshoter (tab):
#     - [x] Enable screenshoter:
#       - Full screenshot: <M-S-5>
#       - Area screenshot: <M-S-6>
#       - Active window screenshot: <M-S-7>
mas 'Cloud Mail.Ru', id: 893068358
# to add Safari extension first open Dashlane, then Safari -
# you'll be prompted to install Dashlane extension
#
# - Preferences:
#   - General:
#     - [x] Start Dashlane at login
mas 'Dashlane - Password Manager, Secure Digital Wallet', id: 552383089
mas 'Evernote', id: 406056744
# - System Preferences:
#   - Security & Privacy -> Privacy -> Accessibility
# - Preferences:
#   - Window Mover:
#     - Move: <S-M> + one finger (+ Left Mouse Button for mouse)
#     - Resize: <S-M> + two fingers (+ Right Mouse Button for mouse)
#   - Layouts:
#     - Maximize: <S-M-CR>
#   - Preferences:
#     - [x] Autostart on system login
#     - [x] Show icon in menu bar
#     - [ ] Show icon in Dock
mas 'Flexiglass', id: 426410278
mas 'FTP Server', id: 987045856
mas 'Ghostery Lite', id: 1436953057
#mas 'iMovie', id: 408981434
#mas 'LINE', id: 539883307
#mas 'Magic Go', id: 1060433592
mas 'Marked 2', id: 890031187
mas 'Microsoft Remote Desktop', id: 715768417
# - Preferences:
#   - General:
#     - After upload:
#       - [ ] Open in browser
#     - [x] Launch at login
#   - Hotkeys:
#     - Capture area: <M-1>
#   - Account (to be able to upload screenshots):
#     - Login -> Login with Facebook (a***.t***.i***@gmail.com)
mas 'Monosnap', id: 540348655
mas 'Pomodoro Timer', id: 872515009
mas 'Screen Recorder Pro - Screen Capture HD Video', id: 985439997
# - System Preferences:
#   - Users & Groups -> Login Items: add (don't hide)
# - Preferences:
#   - Shortcuts:
#     - General -> Show Magnifier: <M-2>
mas 'Sip', id: 507257563
mas 'Telegram Desktop', id: 946399090
mas 'Trello', id: 1278508951
# - System Preferences:
#   - Users & Groups -> Login Items: add (don't hide)
mas 'VPNAutoConnect', id: 532510878
# after installation or update:
# - `sudo xcodebuild -license accept` (accept license)
# - `xcode-select --install` (install CLT)
mas 'Xcode', id: 497799835
mas 'iA Writer', id: 775737590
