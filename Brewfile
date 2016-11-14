# https://github.com/Homebrew/homebrew-bundle

# - some cask packages and App Store applications ask for password
# - see comments before package or application for postinstallation setup
# - see `brew services` on how to manage services for supported forumalae

cask_args appdir: '/Applications'

tap 'beeftornado/rmtree'
tap 'caskroom/cask'
tap 'caskroom/versions'
tap 'homebrew/bundle'
tap 'homebrew/services'
tap 'homebrew/versions'

#brew 'chromedriver'
#brew 'djview4'

# remove formula with its unused dependencies:
# brew rmtree mpv
brew 'beeftornado/rmtree/brew-rmtree'
brew 'elixir'
# required by ~/scripts/fontpatcher
brew 'fontforge'
brew 'git'
brew 'gpg'
brew 'htop'
brew 'imagemagick'
# used as viewer for html files in mc
brew 'lynx'
# - copy fonts from dotfiles
# - ~/.vim/update_bundles
#   (or ~/.dotfiles/.vim/update_bundles if symlinks are not created yet)
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
# for reenter_feed
brew 'nodejs'
brew 'p7zip'
brew 'phantomjs'
brew 'postgresql', restart_service: :changed
# it's much easier to install pow manually:
# - curl get.pow.cx | sh
# - ln -s ~/dev/reenter_builder ~/.pow
#brew 'pow'
# http://tap349.github.io/rbenv/ruby/chef/capistrano/2016/03/30/rbenv/
brew 'rbenv'
brew 'rbenv-ctags'
brew 'redis', restart_service: :changed
brew 'ssh-copy-id'
brew 'the_silver_searcher'
brew 'tree'
brew 'unrar'
brew 'wget'
# - make it a login shell: chsh -s /bin/zsh
#   (all available shells are listed in /etc/shells)
# - install oh-my-zsh with plugins:
#   - https://github.com/zsh-users/zsh-autosuggestions#oh-my-zsh
#   - https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/INSTALL.md#oh-my-zsh
brew 'zsh'

cask 'chefdk'
# - app preferences:
#   - [x] Start f.lux at login (set by default)
cask 'flux'
# - system preferences:
#   - Users & Groups -> Login Items (don't hide): remove
# - app preferences:
#   - Settings -> On startup: Continue where you left off
#   - Extensions (allow all of them in incognito):
#     - Proxy SwitchyOmega (configure proxy per site)
#     - AdBlock
#     - Blank New Tab (Hide in Chrome Menu)
#     - Browsec VPN
#     - Dashlane (not available in webstore - added when opening Dashlane)
#     - Data Saver
#     - Ghostery:
#         - General: don't show purple box
#         - Blocking Options: block all except comments
#     - Google Docs (installed by default)
#     - Google Docs Offline (installed by default)
#     - Google Sheets (installed by default)
#     - JSON Formatter (Hide in Chrome Menu)
#     - Quick Javascript Switcher
#     - Noisli
#     - RSS Subscription Extension (by Google) - optional
#     - The Great Suspender
#     - uBlock Origin
#     - Viewport Dimensions (Hide in Chrome Menu)
#   - Theme (search on page) -> Black Carbon + silver meta
#   - chrome://plugins:
#     - disable Adobe Flash Player
# - bookmarks bar:
#   - pin! (https://pinboard.in/howto/)
cask 'google-chrome'
# - copy fonts from dotfiles
# - app preferences:
#   - General -> Preferences -> Load preferences from a custom folder or URL:
#     /Users/tap/.dotfiles/.config/iterm2
#     (this directory should contain com.googlecode.iterm2.plist settings file
#     exported using 'Save Current Settings to Folder' button below).
#     or else copy ~/Library/Preferences/com.googlecode.iterm2.plist -
#     in both cases iTerm will be fully configured (restart is required)
#
# see also http://tap349.github.io/iterm/rails/2016/05/03/iTerm/
# (all these settings are already included in exported settings file)
cask 'iterm2'
cask 'openemu'
cask 'pgadmin4'
cask 'skype'
# - app preferences:
#   - Menubar: Temperature only
#   - [x] Check for updates on startup
#   - [x] Autostart smcFanControl after login
cask 'smcfancontrol'
# - open /usr/local/Caskroom/utorrent/latest/uTorrent.app
# - system preferences:
#   - Users & Groups -> Login Items (don't hide): remove
cask 'utorrent'

# - link to dropbox account and sync (replace local data)
# - app preferences:
#   - General:
#     - Quick Entry: <C-S-a>
mas '2Do', id: 477670270
# on first run:
# - select ~/Documents folder
#   (Cloud Mail.Ru subfolder will be created automatically)
# - select to start application on system startup
# - select folders to synchronize (books/, videos/, education/)
mas 'Cloud Mail.Ru', id: 893068358
mas 'Dashlane - Password Manager, Secure Digital Wallet', id: 552383089
mas 'Evernote', id: 406056744
# - on first run agree to start Flexiglass every time systems starts
# - system preferences:
#   - Security & Privacy -> Privacy -> Accessibility
# - app preferences:
#   - Window Mover:
#     - Move: <S-M> + one finger (+ Left Mouse Button for mouse)
#     - Resize: <S-M> + two fingers (+ Right Mouse Button for mouse)
#   - Layouts:
#     - Maximize: <S-M-CR>
#   - Preferences:
#     - [ ] Show icon in Dock
mas 'Flexiglass', id: 426410278
mas 'Marked 2', id: 890031187
# - app preferences:
#   - General:
#     - [x] Launch at login
#   - Hotkeys:
#     - Capture area: <M-1>
mas 'Monosnap', id: 540348655
mas 'Pomodoro Timer', id: 872515009
# - system preferences:
#   - Users & Groups -> Login Items (don't hide): add
mas 'VPNAutoConnect', id: 532510878
mas 'Xcode', id: 497799835
# - system preferences:
#   - Users & Groups -> Login Items (don't hide): add
# - app preferences:
#   - Shortcuts:
#     - General -> Show Magnifier: <M-2>
mas 'Sip', id: 507257563
mas 'Telegram Desktop', id: 946399090
