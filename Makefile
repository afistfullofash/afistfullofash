test: test_operating_system test_home test_defuns

test_operating_system:
	echo ""
	echo ""
	echo ""
	echo "Testing: service-test-operating-system"
	guix system container -L . -e \
	"(use-modules (afistfullofash-tests system-services)) service-test-operating-system"

test_home_container:
	guix home container -L . -e \
	"(use-modules (afistfullofash-tests ${TEST_MODULE})) ${TEST_HOME}"


test_home:
	echo ""
	echo ""
	echo ""
	echo "Testing: dunst-service-home-no-config"
	make test_home_container TEST_MODULE=home-services TEST_HOME=dunst-service-home-no-config
	echo ""
	echo ""
	echo ""
	echo "Testing: dunst-service-home-with-config"
	make test_home_container TEST_MODULE=home-services TEST_HOME=dunst-service-home-with-config
	echo ""
	echo ""
	echo ""
	echo "Testing: runst-service-home"
	make test_home_container TEST_MODULE=home-services TEST_HOME=runst-service-home

test_defuns:
	echo ""
	echo ""
	echo ""
	echo "Testing: runst-service-home"
	guix repl -L . -e "(use-modules (afistfullofash-tests home-services)) dunst-service-defun-home-dunst-config-modification"
