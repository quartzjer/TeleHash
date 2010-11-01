/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.json.model.impl.JsObjectImpl;
import org.telehash.model.TelehashObject;
import org.telehash.model.TelehashPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Object</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * </p>
 *
 * @generated
 */
public abstract class TelehashObjectImpl extends JsObjectImpl implements
		TelehashObject {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TelehashObjectImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TelehashPackage.Literals.TELEHASH_OBJECT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public Object getHeader(String key) {
		return get("_" + key);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public Object getCommand(String key) {
		return get("." + key);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public Object getSignal(String key) {
		return get("+" + key);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public TelehashObject withHeader(String key, Object value) {
		return (TelehashObject) with("_" + key, value);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public TelehashObject withCommand(String key, Object value) {
		return (TelehashObject) with("." + key, value);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public TelehashObject withSignal(String key, Object value) {
		return (TelehashObject) with("+" + key, value);
	}

} //TelehashObjectImpl
