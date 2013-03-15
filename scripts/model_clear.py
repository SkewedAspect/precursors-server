import rkit

import models

rkit.connect("localhost", 8081)
print "Deleting EVERYTHING..."

for modelCls in models.__dict__.values():
    if hasattr(modelCls, 'Meta'):
        bucket = rkit.connection.bucket(modelCls.Meta.bucket)
        for key in bucket.get_keys():
            bucket.get(key).delete()

print "Done."
