#!/bin/bash

export PATH="/opt/otp-R15B/bin:$PATH"

set -e

function info() { echo -e "\033[1;32m$@\033[m"; }
function info_nonl() { echo -ne "\033[1;32m$@\033[m"; }
function confirm_continue() {
	read -N 1 CHOICE
	case "$CHOICE" in
		"\r"|"\n"|"\r\n"|"") ;;
		y|Y|yes|Yes|YES) echo ;;
		*) exit 1 ;;
	esac
}


info "This will generate a 4-node Riak development cluster in ./riak"
info_nonl "Do you want to continue? [Y|n]"
confirm_continue


info "Getting and building Riak and dependencies..."

git clone -b riak-1.3.1 git://github.com/basho/riak.git
pushd riak

make all


info "Generating a 4-node dev cluster..."

make devrel DEVNODES=4


info "Changing storage backend to ELevelDB..."

sed -i '/{storage_backend,/s/riak_kv_.*_backend/riak_kv_eleveldb_backend/' dev/dev1/etc/app.config


info "Generating cluster run script..."

cat > riak-dev-cluster <<EOF
#!/bin/bash
set -e
function info() { echo -e "\\033[1;32m\$@\\033[m"; }
RIAKDIR="\$(dirname \$0)"
cd \$RIAKDIR/dev
case "\$1" in
	--first) NODES=("dev1"); NODES_DESC="the first node"; shift; ;;
	--not-first) NODES=("dev"[2-9]); NODES_DESC="all but the first node"; shift; ;;
	*) NODES=("dev"*); NODES_DESC="all nodes"; ;;
esac
case "\$1" in
	admin) CMD=riak-admin; shift; ;;
	cluster) CMD=riak-admin; ;;
	search) CMD=search-cmd; shift; ;;
	*) CMD=riak; ;;
esac
info "Running \\\`\$CMD \$*\\\` on all nodes..."
for node in "\${NODES[@]}"; do
	info "  on \${node}..."
	"\$node/bin/\$CMD" "\$@"
done
EOF

chmod +x riak-dev-cluster


./riak-dev-cluster start


info "Clustering nodes..."

./riak-dev-cluster --not-first cluster join dev1@127.0.0.1
sleep 2

./riak-dev-cluster --first cluster plan
info "The above cluster plan is about to be committed."
info_nonl "Do you want to continue? [Y|n]"
confirm_continue

./riak-dev-cluster --first cluster commit


info "Loading initial data..."

popd
RIAK_PB_PORT=$(sed '/{pb_port,/s/[^0-9]*//gp;d' riak/dev/dev1/etc/app.config)
PYTHON=($(which python2 python2.7 python2.6 python 2>/dev/null || true))
$PYTHON $(dirname $0)/load_initial_db.py $RIAK_PB_PORT


info "Done."
