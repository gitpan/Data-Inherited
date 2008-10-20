use inc::Module::Install;
include 'Module::AutoInstall';

name 'Data-Inherited';
all_from 'lib/Data/Inherited.pm';
perl_version '5.006';

recommends 'Test::Pod';
recommends 'Test::Pod::Coverage';

test_requires 'Test::More' => '0.70';
test_requires 'Test::Compile';

auto_install;
auto_include;
WriteAll;
