use inc::Module::Install;
name 'Data-Inherited';
all_from 'lib/Data/Inherited.pm';
perl_version '5.006';

build_requires 'Test::More' => '0.70';

use_standard_tests(without => 'pod_coverage');
auto_include;
WriteAll;
