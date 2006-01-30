#!/usr/bin/env perl

use warnings;
use strict;
use Project::Build;

Project::Build->new(
    module_name       => 'Data-Inherited',
    dist_version_from => 'lib/Data/Inherited.pm',
    dist_author       => 'Marcel Gruenauer <marcel@noug.at>',
    dist_abstract     => 'hierarchy-wide accumulation of list and hash results',
    requires          => {
        # 'NEXT' => 0.60,   # included in Data::Inherited for now, q.v.
    },
)->create_build_script;
