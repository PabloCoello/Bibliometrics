DM_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
WO_DIR=/opt/bibliometrics
DK_IMG=bibliometrics:latest

while :
	do
	echo "1. Build image"
	echo "2. Run analysis"
	echo "3. Modify configuration"
	echo "4. EXIT"
	echo -n "Choose one option [1 - 4]: "
	read opcion

function build_dev_image () {
	docker build -t "${DK_IMG}" "${DM_DIR}"/.devcontainer/
}

function run_analysis () {
	docker run --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" R -e "rmarkdown::render('bibliometric_analyser.Rmd',params = 'ask',output_file='./Results/Result.html')" run
}

function modify_conf () {
	sudo nano ./conf/conf.json
}


case $opcion in
	1)
		build_dev_image
		;;
	2)
		run_analysis
		;;
	3)
		modify_conf
		;;
	4)
		echo "bye";
		exit 1
		;;
esac
done