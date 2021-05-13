# Copyright (C) 2021 dalu

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

-include lib/borg/borg.mk

help::
	$(info make bootstrap-shallow = shallow bootstrap configurations)

bootstrap-borg:
	@git submodule--helper clone --name borg --path lib/borg \
	--url https://github.com/emacscollective/borg.git
	@cd lib/borg; git symbolic-ref HEAD refs/heads/master
	@cd lib/borg; git reset --hard HEAD

bootstrap-shallow:
	@printf "\n=== Running 'git submodule init' ===\n\n"
	@git submodule init
	@printf "\n=== Running 'borg-shallow.sh' ===\n"
	@./borg-shallow.sh
	@printf "\n=== Running 'make build' ===\n\n"
	@make build
