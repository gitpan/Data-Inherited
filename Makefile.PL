use inc::Module::Install;
name 'Data-Inherited';
all_from 'lib/Data/Inherited.pm';

build_requires 'Test::More';

use_test_base;
auto_include;
WriteAll;
