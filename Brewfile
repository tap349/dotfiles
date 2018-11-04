# https://github.com/Homebrew/homebrew-bundle

# - some cask packages and App Store applications ask for password
# - see comments before package or application for postinstallation setup
# - see `brew services` on how to manage services for supported forumalae

#-------------------------------------------------------------------------------
# Taps (third-party repositories)
#-------------------------------------------------------------------------------

# remove formula with its unused dependencies:
# `brew rmtree mpv`
tap 'beeftornado/rmtree'
tap 'puma/puma'
tap 'caskroom/cask'
tap 'caskroom/fonts'
tap 'caskroom/versions'
tap 'homebrew/bundle'
tap 'homebrew/services'
# for wuzz
tap 'ZloeSabo/homebrew-nettools'

#-------------------------------------------------------------------------------
# Homebrew
#-------------------------------------------------------------------------------

#brew 'chromedriver'
#brew 'djview4'

brew 'ansible'
# after installation:
# - `aws configure`
brew 'awscli'
brew 'bfg'
brew 'ccze'
# for cpsm vim plugin
brew 'cmake'
# for cpsm vim plugin
brew 'boost'
# install manually by downloading dmg file
#brew 'cgoban'
brew 'djvu2pdf'
brew 'elasticsearch'
# install via asdf
#brew 'elixir'
# for ~/scripts/fontpatcher
brew 'fontforge'
# for shikimori
brew 'geoip'
brew 'git'
brew 'gpg'
brew 'honcho'
brew 'htop'
brew 'imagemagick'
brew 'jq'
brew 'kibana'
# provides rsvg-convert utility to convert svg to png
brew 'librsvg'
# used as viewer for html files in mc
brew 'lynx'
# - install vim-plug:
#   `curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim`
# - install plugins:
#   :PlugInstall
#
# - app preferences:
#   - General:
#     - After last window closes: Quit MacVim
#   - Advanced:
#     - [ ] Draw marked text inline
brew 'macvim'
brew 'mas'
brew 'memcached', restart_service: :changed
brew 'mc'
brew 'mpv'
# for React Native
# (same as nodejs)
brew 'node'
brew 'octave'
brew 'p7zip'
# https://github.com/Homebrew/brew/blob/master/docs/Versions.md
# now postgresql@9.5 is run inside docker container
#brew 'postgresql@9.5', restart_service: :changed
# for psql
brew 'postgresql', restart_service: :changed
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
# http://tap349.github.io/rbenv/ruby/chef/capistrano/2016/03/30/rbenv
brew 'rabbitmq'
brew 'rbenv'
brew 'rbenv-ctags'
brew 'redis', restart_service: :changed
brew 'ripgrep'
brew 'ssh-copy-id'
brew 'tig'
brew 'tmux'
brew 'tree'
brew 'unrar'
# for React Native
brew 'watchman'
brew 'wget'
brew 'wuzz'
brew 'yarn'
# - make it a login shell: `chsh -s /bin/zsh`
#   (all available shells are listed in /etc/shells,
#   current shell can be printed with `echo $0` command)
# - install oh-my-zsh:
#   - https://github.com/robbyrussell/oh-my-zsh#via-curl
# - install oh-my-zsh plugins:
#   - https://github.com/zsh-users/zsh-autosuggestions#oh-my-zsh
#   - https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/INSTALL.md#oh-my-zsh
brew 'zsh'

#-------------------------------------------------------------------------------
# Homebrew-Cask
#-------------------------------------------------------------------------------

cask_args appdir: '/Applications'

# for Raspeberry Pi
#cask 'applepi-baker'
cask 'calibre'
# install required gems (knife-zero, chef, berkshelf) using Gemfile
#cask 'chefdk'
# - app preferences:
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
# for elasticsearch in iceperk
#
# run docker app after installation
# (otherwise `docker` command might be not available in terminal)
#
# - system preferences:
#   - Users & Groups -> Login Items: add (don't hide)
cask 'docker'
#cask 'firefox'
# - system preferences:
#   - Users & Groups -> Login Items: add (hide)
# - app preferences:
#   - 'Working late' preset
#   - [x] Start f.lux at login (set by default)
cask 'flux'
cask 'font-fontawesome'
cask 'font-cousine'
cask 'font-d2coding'
cask 'font-droid-sans-mono'
cask 'font-fira-mono'
cask 'font-inconsolata-dz'
# for iTerm2
cask 'font-inconsolata-lgc'
cask 'font-iosevka'
cask 'genymotion'
# app preferences (bookmarks bar, extensions, etc.) are synchronized
# if you sign in to Chrome (my email is a***.t***.i***@gmail.com)
#
# on first run:
#  - [x] Set Google Chrome as your default browser
#  - [ ] Help make Google Chrome better by automatically sending usage
#        statistics and crash reports to Google
#  - Sign in to Chrome (page is opened automatically)
#
# - system preferences:
#   - Users & Groups -> Login Items: remove (don't hide)
# - app preferences:
#   - Settings -> On startup: Continue where you left off
#   - Extensions (allow all of them in incognito):
#     + AdBlock (Hide in Chrome Menu)
#       - [ ] Show number of ads blocked on AdBlock button
#     + Blank New Tab (Hide in Chrome Menu)
#     + Browsec VPN
#     + Dashlane (select Desktop mode, can be added on first Dashlane run)
#     - Data Saver (turn off - turned on by default)
#     + Ghostery (Hide in Chrome Menu):
#       - Global Blocking:
#         - Block All (except Comments)
#       - Notifications:
#         - [ ] Show tracker count badge on the Ghostery icon in browser toolbar
#       - Purple Box:
#         - [ ] Show the purple box in the corner of my browser
#     + Google Docs Offline (installed by default)
#     + GoUniverse (Hide in Chrome Menu)
#     - JSON Formatter (Hide in Chrome Menu)
#     + JSON Viewer (Hide in Chrome Menu)
#       - Theme: mdn-like
#       - Structure:
#         - "lineNumbers": false
#     + Music Downloader Professional (Hide in Chrome Menu)
#     + Noisli (Hide in Chrome Menu)
#     - Proxy SwitchyOmega (configure proxy per site)
#     - Quick Javascript Switcher (Hide in Chrome Menu)
#     + React Developer Tools (Hide in Chrome Menu)
#     + Scrum for Trello (Hide in Chrome Menu)
#     - Tag Assistant (by Google) (enabled per tab)
#     + The Great Suspender
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
# - app preferences:
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
# for android-sdk
# https://github.com/facebook/react-native/issues/17688
cask 'java8'
# - system preferences:
#   - Users & Groups -> Login Items: remove
cask 'keybase'
cask 'ngrok'
cask 'openemu'
cask 'opera'
cask 'pgadmin4'
cask 'phantomjs'
cask 'psequel'
cask 'react-native-debugger'
cask 'skype'
# - app preferences:
#   - Menubar: Temperature only
#   - [x] Check for updates on startup
#   - [x] Autostart smcFanControl after login
cask 'smcfancontrol'
# - system preferences:
#   - Users & Groups -> Login Items: remove (don't hide)
cask 'steam'
cask 'unetbootin'
# - `open /usr/local/Caskroom/utorrent/latest/uTorrent.app`
# - system preferences:
#   - Users & Groups -> Login Items: remove (don't hide)
cask 'utorrent'
cask 'whatsapp'

#-------------------------------------------------------------------------------
# App Store
#
# mas search Trello
#-------------------------------------------------------------------------------

# - link to dropbox account and sync (Replace Local Data)
# - app preferences:
#   - General:
#     - Quick Entry: <C-S-a>
#   - Appearance:
#     - Font Size: Big
#     - [ ] Show notes under tasks
#   - Sync:
#     - Setup:
#       - Link Dropbox Account
mas '2Do', id: 477670270
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
# - app preferences:
#   - General:
#     - [x] Start Dashlane at login
mas 'Dashlane - Password Manager, Secure Digital Wallet', id: 552383089
mas 'Evernote', id: 406056744
# - system preferences:
#   - Security & Privacy -> Privacy -> Accessibility
# - app preferences:
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
mas 'Marked 2', id: 890031187
mas 'Microsoft Remote Desktop', id: 715768417
# - app preferences:
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
# - system preferences:
#   - Users & Groups -> Login Items: add (don't hide)
# - app preferences:
#   - Shortcuts:
#     - General -> Show Magnifier: <M-2>
mas 'Sip', id: 507257563
mas 'Telegram Desktop', id: 946399090
mas 'Trello', id: 1278508951
# - system preferences:
#   - Users & Groups -> Login Items: add (don't hide)
mas 'VPNAutoConnect', id: 532510878
# after installation or update:
# - `sudo xcodebuild -license accept` (accept license)
# - `xcode-select --install` (install CLT)
mas 'Xcode', id: 497799835
