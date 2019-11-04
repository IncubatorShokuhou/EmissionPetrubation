## 排放源扰动程序

它能在wrfchem原有的排放源的基础上，生成若干组扰动的排放源。

排放源扰动程序的原始wrfchem排放源位于`./origin/`文件夹下。其中`1level`文件夹为垂直分层为1层的排放源文件。

排放源扰动程序的主要程序位于`./main/`文件夹下。直接运行`./run.sh`脚本即可执行。在运行前，首先需要确定netcdf库的位置：

> export NETCDF=你的netcdf库位置

首先，输入需要生成的排放源组的个数。该个数必须为1~999之间的整数。接着，脚本会运行文件夹下的`create_dirs.pl`脚本，生成若干个文件夹，并将原始wrfchem排放源复制入文件夹中。

接着，编译并运行`wrfchem.f90`脚本。该脚本是源扰动程序的主要部分。它使用`mt19937ar.f90`的Module，首先生成了一组满足标准正态分布的随机数pfactors(Nmembers)，其中Nmembers为上文确定的排放源组的个数。接着，对排放源文件中各个排放源变量进行扰动。根据分布类型的不同，扰动可分为正态分布和对数正态分布两种。分布类型可在运行run.sh脚本过程中，通过屏幕输入。最后，将经过扰动的变量赋值回其原本所在的nc文件中。

需要注意的是，在运行和编译wrfchem.f90脚本之前，需要对其进行一定修改。

> INTEGER(4),PARAMETER :: south_north=99
> INTEGER(4),PARAMETER :: west_east=159

上述两参数需要与实际的排放源文件中的经纬度维度相同。可以使用

> ncdump -h ncdump -h wrfchemi_d01_2016-11-01_00:00:00.nc

查看该nc文件的文件头，并确定该两个参数的值。

另外，当对多个不同月份，使用同样的扰动参数，同时进行扰动时，需要修改

> DO NNmonthsNN=11,12,1 ! determine the start and end month here

这一行。其中11,12分别为开始和结束的月份。

当wrfchem.f90编译并执行完成后，在./目录下每个文件夹内，应当会有Nmonths个nc文件（Nmonths为上一段所述多个月份的排放源文件）。如果需要生成一个时间序列的排放源文件，则需要编译并执行`changedate*.f90`文件。该文件可以修改以复制多份并重命名的nc文件中的时间变量（复制和重命名的脚本位于`copy*.sh`中，已由上述perl脚本复制入对应文件夹中），使它能适用于当wrfchem的namelist.input中io_style_emissions=2的情景。需要说明的是，若该月份天数不足31天，需要手动删除不存在的该月31日等日期。

本程序只使用于emiss_opt= 4的源文件的扰动。如果需要对其他源进行扰动，可以参照`wrfchem.f90`脚本自行编写。

`nc2f90.f90`脚本可帮助生成读取排放源nc文件的.f90文件。编译后，执行`./nc2f90 input_file`即可生成名为`rd_netcdf_model.f90`的文件。可在此文件和`wrfchemi.f90`的基础上进行相应修改。

下图为生成的扰动源的空间分布的效果对比。
![Image text](https://github.com/IncubatorShokuhou/EmissionPetrubation/blob/master/4_emissions.png)
