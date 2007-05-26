#!/usr/bin/env python
# encoding: utf-8
"""
metadata.py

Created by Maximillian Dornseif on 2007-01-21.
Copyright (c) 2007 HUDORA GmbH.
"""

import sqlalchemy
from sqlalchemy import DynamicMetaData
from sqlalchemy.orm.mapper import global_extensions
from sqlalchemy.ext.sessioncontext import SessionContext

context = SessionContext(sqlalchemy.create_session)
global_extensions.append(context.mapper_extension)

metadata = DynamicMetaData()
